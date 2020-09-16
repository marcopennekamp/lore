package lore.compiler.phases.resolution

import lore.compiler.syntax.TypeDeclNode
import lore.compiler.core.{Compilation, CompilationException, Error}
import lore.compiler.core.Compilation.Verification
import lore.compiler.phases.resolution.DependencyGraph.InheritanceCycle
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.Graph

/**
  * A dependency graph for declared types that is used by [[DeclarationResolver]] to compute the correct order
  * in which classes should be created.
  */
class DependencyGraph(owner: DependencyGraph.Owner) {

  // TODO: Since component types are resolved right away, we have to add additional edges to the dependency graph, for
  //       example from a struct A to a trait T which extends +A. The question is whether this will work and we can
  //       find a good order in which to compile traits and structs, or whether we have to defer the resolution of
  //       component types until all traits and structs have been resolved. The latter would be necessary in cases
  //       where even just some of the idiomatic Lore programs (such as existing examples) wouldn't compile because of
  //       this.

  /**
    * A mutable dependency graph, the nodes being type names. A type Any is the root of all declared types. Edges are
    * directed from supertype to subtype.
    */
  private val graph: Graph[String, DiEdge] = Graph("Any")
  private implicit val edgelordFactory = DiEdge

  /**
    * Adds a dependency relationship from the dependant type to the type it depends on.
    */
  def add(dependant: String, dependency: String): Unit = graph.addEdge(dependency, dependant)

  /**
    * Calculates the order in which classes need to be resolved.
    */
  def computeTypeResolutionOrder(): Compilation[Vector[String]] = {
    removeNonExistentTypes()
    for {
      _ <- verifyNotCyclical()

      // Now that spurious names have been removed and the graph has been shown not to have any cycles, since Any should
      // be the supertype of all declared types without a supertype, the graph should be connected. Note that we first
      // need to detect cycles, because if the graph has a dependency cycle, that component of the graph will not be
      // connected to Any, and thus the graph won't be connected.
      _ = assert(graph.isConnected)

      // At this point, we know our dependency graph is a directed, acyclic graph. We can start a topological sort.
      order = graph.topologicalSort.fold(
        _ => throw CompilationException(
          "Topological sort on the dependency graph found a cycle, even though we verified earlier that there was no such cycle."
        ),
        order => order.toVector.map(_.value)
      )

      // The first element in the type resolution order must be Any, as it's the ultimate root type.
      _ = assert(order.head == "Any")
    } yield order
  }

  /**
    * Once the dependency graph has been built, not all types in the graph will be valid. As supertype declarations
    * can refer to non-existent types by name, we have to take care that all names added to the graph without a
    * corresponding type declaration must be removed again.
    */
  private def removeNonExistentTypes(): Unit = {
    for (node <- graph.nodes) {
      if (node.value != "Any" && !owner.hasTypeDeclaration(node.value)) {
        // The type was never declared and is thus invalid! We cannot, however, simply remove the node. The node will
        // have a dependant which should rather be Any for the purposes of this graph. For example, let's say we have
        // a type A that extends a type B. B doesn't exist. Then A should, for the purposes of cycle detection, derive
        // from Any.
        for (edge <- node.edges) {
          if (!(edge.from == node)) {
            throw CompilationException("An undeclared type should not depend on any types in the dependency graph.")
          }
          val dependant = edge.to
          add(dependant, "Any")
        }
        graph.remove(node.value)
      }
    }
  }

  /**
    * Verify that this dependency graph doesn't contain any cycles, which would mean that at least two classes
    * inherit from each other.
    *
    * We attempt to report as many cycles as possible so the user doesn't have to run the compiler multiple times
    * just to find all dependency cycles. However, we cannot guarantee that all cycles are found in a single run.
    */
  private def verifyNotCyclical(): Verification = {
    if (graph.isCyclic) {
      val cycles = distinctCycles
      assert(cycles.nonEmpty)
      return Compilation.fail(
        cycles.map { cycle =>
          val occurrence = owner.getTypeDeclaration(cycle.startNode).getOrElse(
            throw CompilationException("Type declarations didn't contain a declaration that was part of the dependency graph."),
          )
          InheritanceCycle(cycle.nodes.map(_.value).toVector, occurrence)
        }: _*
      )
    }
    Verification.succeed
  }

  /**
    * Find multiple cycles in the graph. Cycles with the same elements and order, but different starting nodes, are
    * only returned once. The function is useful for finding multiple dependency cycles so that users can quickly
    * fix them.
    *
    * This function is not guaranteed to find ALL cycles, but there is a good chance that it does.
    */
  private def distinctCycles: Vector[graph.Cycle] = {
    var cycles = Vector.empty[graph.Cycle]
    for (node <- graph.nodes) {
      node.partOfCycle.foreach { cycle =>
        // We have found a cycle. Now we need to ensure that it isn't in the list yet.
        if (!cycles.exists(_.sameAs(cycle))) {
          cycles = cycles :+ cycle
        }

        // This is not the most efficient way to compare cycles. We are operating on the assumption that dependency
        // cycles will be a rare occurrence: if a cycle happens, the programmer is inclined to fix it right away.
        // In the current implementation, any new cycle inserted into the list has to be compared to all other cycles.
        // That is fine if we don't find more than 100 cycles or so. Should we ever have performance issues stemming
        // from this section of the code, we can introduce a hashing function that is stable in respect to a cycle's
        // starting node, and thus likely throws only same cycles into the same bucket.
      }
    }
    cycles
  }

}

object DependencyGraph {
  /**
    * The owner of a dependency graph, providing a view on the body of type declarations known to the compiler.
    */
  trait Owner {
    def hasTypeDeclaration(name: String): Boolean
    def getTypeDeclaration(name: String): Option[TypeDeclNode]
  }

  /**
    * @param occurrence One of the type declarations where the cycles occurs, so that we can report one error location.
    */
  case class InheritanceCycle(cycle: Vector[String], occurrence: TypeDeclNode) extends Error(occurrence) {
    override def message: String =
      s"""An inheritance cycle between the following types has been detected: ${cycle.mkString(", ")}.
         |A trait cannot inherit from another trait that also inherits from the former directly or indirectly. The
         |subtyping and component relationships of declared types must result in a directed, acyclic graph."""
        .stripMargin.replaceAll("\n", " ").trim
  }
}
