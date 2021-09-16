package lore.compiler.resolution

import lore.compiler.core.CompilationException
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.resolution.DeclarationResolver.TypeDeclarations
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.semantics.{NamePath, Registry}
import lore.compiler.syntax.TypeExprNode.TypeNameNode
import lore.compiler.syntax.{DeclNode, TypeDeclNode, TypeExprNode}
import lore.compiler.types.{BasicType, Type}
import lore.compiler.utils.CollectionExtensions.VectorExtension
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.config.CoreConfig
import scalax.collection.immutable.Graph

object TypeDependencies {

  private case class TypeDeclarationInfo(node: TypeDeclNode, dependencies: Vector[NamePath])

  /**
    * Verifies that the dependencies between the given types are correct and computes an order in which schemas need to
    * be resolved.
    *
    * Type dependencies are correct when:
    *   1. All type dependencies referred to by name have a corresponding type declaration.
    *   2. There are no cyclic type dependencies.
    */
  def resolve(typeDeclarations: TypeDeclarations)(implicit reporter: Reporter): Registry.SchemaResolutionOrder = {
    val unfilteredInfos = typeDeclarations.values.toVector.map(node => TypeDeclarationInfo(node, dependencies(node)))
    val infos = unfilteredInfos.map(filterUndefinedDependencies(_, typeDeclarations))
    var graph = buildDependencyGraph(infos)

    graph = filterCycles(graph, typeDeclarations)
    graph = reconnectRoots(graph)

    // Now that the graph has been shown to be acyclic, it should be connected, as Any should be the supertype of all
    // declared types without a supertype. Note that we first need to detect cycles, because if the graph has a
    // dependency cycle, that component of the graph will not be connected to Any, and thus the graph won't be
    // connected.
    if (!graph.isConnected) {
      throw CompilationException(s"The type dependency graph must be connected.")
    }

    computeSchemaResolutionOrder(graph)
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
      case DeclNode.AliasNode(_, _, expr, _) => names(expr)
      case DeclNode.StructNode(_, _, _, extended, _, _) => extended.flatMap(names).toSet
      case DeclNode.TraitNode(_, _, extended, _) => extended.flatMap(names).toSet
    }

    // We also have to remove all predefined types from the list because they aren't part of the schema hierarchy.
    (
      (boundNames ++ restNames) -- Type.predefinedTypes.keys
    ).toVector
  }

  case class UndefinedDependency(node: TypeDeclNode, dependency: NamePath) extends Feedback.Error(node) {
    override def message: String = s"The type ${node.fullName} depends on a type $dependency, but it doesn't exist."
  }

  /**
    * Filters out any dependencies without a corresponding type declaration, reporting an error for each undefined
    * dependency. This ensures that the type resolution order contains only defined types.
    */
  private def filterUndefinedDependencies(info: TypeDeclarationInfo, typeDeclarations: TypeDeclarations)(implicit reporter: Reporter): TypeDeclarationInfo = {
    val (defined, undefined) = info.dependencies.partition(typeDeclarations.contains)
    undefined.foreach(dependency => reporter.report(UndefinedDependency(info.node, dependency)))
    info.copy(dependencies = defined)
  }

  private type DependencyGraph = Graph[NamePath, DiEdge]

  /**
    * Builds the dependency graph which is used to find dependency cycles and compute the type resolution order.
    */
  private def buildDependencyGraph(infos: Vector[TypeDeclarationInfo]): DependencyGraph = {
    val edges = infos.flatMap(buildDependencyEdges)
    implicit val config: Graph.Config = CoreConfig()
    Graph.from(edges)
  }

  private def buildDependencyEdges(info: TypeDeclarationInfo): Vector[DiEdge[NamePath]] = {
    info.dependencies
      .map(dependency => dependency ~> info.node.fullName)
      .withDefault(BasicType.Any.name ~> info.node.fullName)
  }

  /**
    * @param occurrence One of the type declarations where the cycles occurs, so that we can report one error location.
    */
  case class InheritanceCycle(typeNames: Vector[NamePath], occurrence: TypeDeclNode) extends Feedback.Error(occurrence) {
    override def message: String =
      s"""An inheritance cycle between the following types has been detected: ${typeNames.mkString(", ")}.
         |A declared type must not inherit from itself directly or indirectly. The subtyping relationships of declared
         |types must result in a directed, acyclic graph."""
        .stripMargin.replaceAll("\n", " ").trim
  }

  /**
    * Removes all types contained in a cycle from the dependency graph, reporting an error for each such cycle. A cycle
    * means that at least one declared type extends itself directly or indirectly.
    */
  private def filterCycles(graph: DependencyGraph, typeDeclarations: TypeDeclarations)(implicit reporter: Reporter): DependencyGraph = {
    var result = graph
    while (result.isCyclic) {
      val cycles = distinctCycles(result)
      if (cycles.isEmpty) {
        throw CompilationException(s"If the graph is cyclic, we must be able to find at least one distinct cycle.")
      }

      cycles.foreach { cycle =>
        val typeNames = cycle.nodes.map(_.value).toVector.init
        // Report the error at each occurrence so that IDEs can properly report them at each location.
        reporter.report(typeNames.map(occurrence => InheritanceCycle(typeNames, typeDeclarations(occurrence))))
        result = result -- typeNames
      }
    }

    result
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
  private def computeSchemaResolutionOrder(graph: DependencyGraph): Vector[NamePath] = {
    val order = graph.topologicalSort.fold(
      _ => throw CompilationException(
        "Topological sort on the type dependency graph found a cycle, even though we verified earlier that there was no such cycle."
      ),
      order => order.toVector.map(_.value)
    )

    if (order.head != BasicType.Any.name) {
      throw CompilationException("The first element in the type resolution order must be the root type Any.")
    }

    // With .tail we exclude Any.
    order.tail
  }

}
