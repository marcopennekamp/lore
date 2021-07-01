package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.feedback.Feedback
import lore.compiler.phases.resolution.DeclarationResolver.TypeDeclarations
import lore.compiler.semantics.Registry
import lore.compiler.syntax.{TypeDeclNode, TypeExprNode}
import lore.compiler.types.{BasicType, Type}
import lore.compiler.utils.CollectionExtensions.VectorExtension
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.config.CoreConfig
import scalax.collection.immutable.Graph

object TypeDependencies {

  /**
    * Verifies that the dependencies between the given types are correct and computes an order in which types need to
    * be resolved.
    *
    * Type dependencies are correct when:
    *   1. All type dependencies referred to by name have a corresponding type declaration.
    *   2. There are no cyclic type dependencies.
    */
  def resolve(typeDeclarations: TypeDeclarations): Compilation[Registry.TypeResolutionOrder] = {
    val infos = typeDeclarations.values.toVector.map(node => TypeDeclarationInfo(node, dependencies(node)))

    for {
      _ <- infos.map(verifyDependenciesExist(_, typeDeclarations)).simultaneous

      graph = buildDependencyGraph(infos)
      _ <- verifyAcyclic(graph, typeDeclarations)

      // Now that the graph has been shown to be acyclic, it should be connected, as Any should be the supertype of all
      // declared types without a supertype. Note that we first need to detect cycles, because if the graph has a
      // dependency cycle, that component of the graph will not be connected to Any, and thus the graph won't be
      // connected.
      _ = if (!graph.isConnected) {
        throw CompilationException(s"The type dependency graph must be connected.")
      }
    } yield computeTypeResolutionOrder(graph)
  }

  private case class TypeDeclarationInfo(node: TypeDeclNode, dependencies: Vector[String])

  private def dependencies(node: TypeDeclNode): Vector[String] = {
    val allNames = node match {
      case TypeDeclNode.AliasNode(_, expr, _) => TypeExprNode.identifiers(expr)
      case TypeDeclNode.StructNode(_, extended, _, _) => extended.flatMap(TypeExprNode.identifiers).toSet
      case TypeDeclNode.TraitNode(_, extended, _) => extended.flatMap(TypeExprNode.identifiers).toSet
    }

    // We have to filter out predefined types, as they may be mentioned in alias types or extended shape types,
    // are not true dependencies.
    allNames.filterNot(Type.predefinedTypes.contains).toVector
  }

  case class UndefinedDependency(node: TypeDeclNode, dependency: String) extends Feedback.Error(node) {
    override def message: String = s"The type ${node.name} depends on a type $dependency, but it doesn't exist."
  }

  /**
    * Verifies that the given dependencies have a corresponding type declaration.
    */
  private def verifyDependenciesExist(info: TypeDeclarationInfo, typeDeclarations: TypeDeclarations): Verification = {
    info.dependencies.map { dependency =>
      if (typeDeclarations.contains(dependency)) Verification.succeed
      else Compilation.fail(UndefinedDependency(info.node, dependency))
    }.simultaneous.verification
  }

  private type DependencyGraph = Graph[String, DiEdge]

  /**
    * Builds the dependency graph which is used to find dependency cycles and compute the type resolution order.
    */
  private def buildDependencyGraph(infos: Vector[TypeDeclarationInfo]): DependencyGraph = {
    val edges = infos.flatMap(buildDependencyEdges)
    implicit val config: Graph.Config = CoreConfig()
    Graph.from(edges)
  }

  private def buildDependencyEdges(info: TypeDeclarationInfo): Vector[DiEdge[String]] = {
    info.dependencies
      .map(dependency => dependency ~> info.node.name)
      .withDefault(BasicType.Any.name ~> info.node.name)
  }

  /**
    * @param occurrence One of the type declarations where the cycles occurs, so that we can report one error location.
    */
  case class InheritanceCycle(cycle: Vector[String], occurrence: TypeDeclNode) extends Feedback.Error(occurrence) {
    override def message: String =
      s"""An inheritance cycle between the following types has been detected: ${cycle.mkString(", ")}.
         |A trait A cannot inherit from another trait B if B also inherits from A directly or indirectly. The
         |subtyping relationships of declared types must result in a directed, acyclic graph."""
        .stripMargin.replaceAll("\n", " ").trim
  }

  /**
    * Verifies that this dependency graph doesn't contain any cycles, which would mean that at least two declared types
    * inherit from each other.
    *
    * We attempt to report as many cycles as possible so the user doesn't have to run the compiler multiple times
    * just to find all dependency cycles. However, we cannot guarantee that all cycles are found in a single run.
    */
  private def verifyAcyclic(graph: DependencyGraph, typeDeclarations: TypeDeclarations): Verification = {
    if (graph.isCyclic) {
      val cycles = distinctCycles(graph)
      if (cycles.isEmpty) {
        throw CompilationException(s"If the graph is cyclic, we must be able to find at least one distinct cycle.")
      }

      val errors = cycles.map(cycle => InheritanceCycle(cycle.nodes.map(_.value).toVector, typeDeclarations(cycle.startNode)))
      Compilation.fail(errors: _*)
    } else Verification.succeed
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
    * Computes the order in which types need to be resolved.
    */
  private def computeTypeResolutionOrder(graph: DependencyGraph): Vector[String] = {
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