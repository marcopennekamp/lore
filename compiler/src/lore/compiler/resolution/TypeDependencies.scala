package lore.compiler.resolution

import lore.compiler.core.CompilationException
import lore.compiler.feedback.{Reporter, SchemaFeedback}
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.definitions.TypeDefinition
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.syntax.TypeExprNode.TypeNameNode
import lore.compiler.syntax.{DeclNode, TypeDeclNode, TypeExprNode}
import lore.compiler.types.{BasicType, Type}
import lore.compiler.utils.CollectionExtensions.VectorExtension
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.config.CoreConfig
import scalax.collection.immutable.Graph

// TODO (multi-import): Rename SchemaResolutionOrder to SchemaInitializationOrder. Also take into account comments that
//                      use the term "schema resolution order".

object TypeDependencies {

  type SchemaResolutionOrder = Vector[TypeDefinition]

  private case class TypeDeclarationInfo(tpe: TypeDefinition, dependencies: Vector[NamePath])

  /**
    * Verifies that the dependencies between the given types are correct and computes an order in which schemas need to
    * be resolved (initialized). In addition, returns a list of cyclic type definitions which haven't been added to the
    * schema resolution order. These types will have to be fallback-initialized separately.
    *
    * Type dependencies are correct when:
    *   1. All type dependencies referred to by name have a corresponding type declaration.
    *   2. There are no cyclic type dependencies.
    */
  def resolve(
    typeDefinitions: Map[NamePath, TypeDefinition],
  )(implicit reporter: Reporter): (SchemaResolutionOrder, Vector[TypeDefinition]) = {
    val unfilteredInfos = typeDefinitions.values.toVector.map(
      tpe => TypeDeclarationInfo(tpe, dependencies(tpe.node))
    )
    val infos = unfilteredInfos.map(filterUndefinedDependencies(_, typeDefinitions))
    var graph = buildDependencyGraph(infos)

    val (filteredGraph, cyclicDefinitions) = filterCycles(graph, typeDefinitions)
    graph = filteredGraph
    graph = reconnectRoots(graph)

    // Now that the graph has been shown to be acyclic, it should be connected, as Any should be the supertype of all
    // declared types without a supertype. Note that we first need to detect cycles, because if the graph has a
    // dependency cycle, that component of the graph will not be connected to Any, and thus the graph won't be
    // connected.
    if (!graph.isConnected) {
      throw CompilationException(s"The type dependency graph must be connected.")
    }

    val order = computeSchemaResolutionOrder(graph, typeDefinitions)
    (order, cyclicDefinitions.toVector)
  }

  private def dependencies(declNode: TypeDeclNode): Vector[NamePath] = {
    val localModule: LocalModule = declNode.localModule
    val typeVariableNames = declNode.typeVariables.map(_.name)

    def names(typeExprNode: TypeExprNode): Set[NamePath] = {
      TypeExprNode.leaves(typeExprNode)
        .filterType[TypeNameNode]
        .map(_.namePath)
        // We have to remove all type variable names from the name path list, because they are declared locally and
        // thus cannot be depended on.
        .filterNot(namePath => namePath.isSingle && typeVariableNames.contains(namePath.simpleName))
        .flatMap(localModule.toAbsoluteTypePath)
        .toSet
    }

    val boundNames = declNode.typeVariables
      .flatMap(tv => tv.lowerBound.toVector ++ tv.upperBound.toVector)
      .flatMap(names)
      .toSet

    val restNames = declNode match {
      case DeclNode.AliasNode(_, _, _, expr, _) => names(expr)
      case DeclNode.StructNode(_, _, _, extended, _, _) => extended.flatMap(names).toSet
      case DeclNode.TraitNode(_, _, extended, _) => extended.flatMap(names).toSet
    }

    // We also have to remove all predefined types from the list because they aren't part of the schema hierarchy.
    (
      (boundNames ++ restNames) -- Type.predefinedTypes.keys
    ).toVector
  }


  /**
    * Filters out any dependencies without a corresponding type declaration, reporting an error for each undefined
    * dependency. This ensures that the type resolution order contains only defined types.
    */
  private def filterUndefinedDependencies(
    info: TypeDeclarationInfo,
    typeDefinitions: Map[NamePath, TypeDefinition],
  )(implicit reporter: Reporter): TypeDeclarationInfo = {
    val (defined, undefined) = info.dependencies.partition(typeDefinitions.contains)
    undefined.foreach(dependency => reporter.report(SchemaFeedback.UndefinedDependency(info.tpe, dependency)))
    info.copy(dependencies = defined)
  }

  private type DependencyGraph = Graph[NamePath, DiEdge]

  /**
    * Builds the dependency graph which is used to find dependency cycles and compute the type resolution order.
    */
  private def buildDependencyGraph(infos: Vector[TypeDeclarationInfo]): DependencyGraph = {
    val edges = infos.flatMap(buildDependencyEdges)
    implicit val config: Graph.Config = CoreConfig()
    Graph.from(Vector(BasicType.Any.name), edges)
  }

  private def buildDependencyEdges(info: TypeDeclarationInfo): Vector[DiEdge[NamePath]] = {
    info.dependencies
      .map(dependency => dependency ~> info.tpe.name)
      .withDefault(BasicType.Any.name ~> info.tpe.name)
  }

  /**
    * Removes all types contained in a cycle from the dependency graph, reporting an error for each such cycle. A cycle
    * means that at least one declared type extends itself directly or indirectly.
    *
    * The type definitions involved in cycles are returned separately.
    */
  private def filterCycles(
    graph: DependencyGraph,
    typeDefinitions: Map[NamePath, TypeDefinition],
  )(implicit reporter: Reporter): (DependencyGraph, Set[TypeDefinition]) = {
    var result = graph
    var cyclicDefinitions = Set.empty[TypeDefinition]

    while (result.isCyclic) {
      val cycles = distinctCycles(result)
      if (cycles.isEmpty) {
        throw CompilationException(s"If the graph is cyclic, we must be able to find at least one distinct cycle.")
      }

      cycles.foreach { cycle =>
        // Report the error at each occurrence so that IDEs can properly report them at each location. Also collect all
        // cyclic definitions.
        val typeNames = cycle.nodes.map(_.value).init.toVector
        typeNames.foreach { typeName =>
          val definition = typeDefinitions(typeName)
          reporter.report(SchemaFeedback.InheritanceCycle(typeNames, definition))
          cyclicDefinitions += definition
        }
        result = result -- typeNames
      }
    }

    (result, cyclicDefinitions)
  }

  /**
    * Finds multiple cycles in the graph. Cycles with the same elements and order, but different starting nodes, are
    * considered the same and only returned once. The function is useful for finding multiple dependency cycles so that
    * users can quickly fix them.
    *
    * This function is not guaranteed to find ALL cycles, but there is a good chance that it does.
    *
    * This is likely not the most efficient way to find cycles. We are operating on the assumption that dependency
    * cycles will be a rare occurrence: if a cycle happens, the program is invalid and the programmer is inclined to
    * fix it right away.
    */
  private def distinctCycles(graph: DependencyGraph): Vector[graph.Cycle] = {
    graph.nodes.toVector
      .flatMap(node => node.partOfCycle)
      .distinctUsing { case (c1, c2) => c1.sameAs(c2) }
  }

  /**
    * Reconnects all declared types not connected to Any with Any. This operation might be necessary after removing
    * nodes from the graph, e.g. when cycles are removed.
    */
  private def reconnectRoots(graph: DependencyGraph): DependencyGraph = {
    graph.nodes.filter(_.inDegree == 0).toVector.map(_.value).filter(_ != BasicType.Any.name).foldLeft(graph) {
      case (graph2, typeName) => graph2.incl(BasicType.Any.name ~> typeName)
    }
  }

  /**
    * Computes the order in which schemas need to be resolved.
    */
  private def computeSchemaResolutionOrder(
    graph: DependencyGraph,
    typeDefinitions: Map[NamePath, TypeDefinition],
  ): Vector[TypeDefinition] = {
    val order = graph.topologicalSort.fold(
      _ => throw CompilationException(
        "Topological sort on the type dependency graph found a cycle, even though we verified earlier that there was" +
          " no such cycle."
      ),
      order => order.toVector.map(_.value)
    )

    if (!order.headOption.contains(BasicType.Any.name)) {
      throw CompilationException("The first element in the type resolution order must be the root type Any.")
    }

    // With .tail we exclude Any.
    order.tail.map(typeDefinitions)
  }

}
