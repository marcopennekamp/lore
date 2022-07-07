package lore.compiler.semantics.functions

import lore.compiler.core.CompilationException
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc

import scala.collection.immutable.HashSet

object DispatchHierarchyBuilder {

  /**
    * Builds the function hierarchy that is used to calculate a multi-functions fit and min sets.
    *
    * Note that this algorithm assumes that there is no pair of functions which are equally specific. Such a
    * user error flaw should be handled elsewhere (for example enforcing uniqueness).
    */
  def build(mf: MultiFunctionDefinition): DispatchHierarchy = {
    var graph: Graph[FunctionDefinition, DiEdge] = Graph()
    def asHierarchy = DispatchHierarchy(graph)
    var remaining = HashSet[FunctionDefinition](mf.functions: _*)

    // At first, all functions are in the `remaining` set. From this set, we successively add functions to the
    // hierarchy that don't have a superfunction in the same set. To find the correct edges to draw for a new function
    // `f`, we calculate the min set given the hierarchy at that time. The min set will represent all functions which
    // can are immediate dispatch predecessors to `f`. This requires that all of `f`'s superfunctions are already in
    // the hierarchy and not the `remaining` set. If the min set is empty, `f` has no dispatch predecessors and becomes
    // a dispatch root.
    while (remaining.nonEmpty) {
      // All unused functions that don't have a superfunction in the remaining set.
      val top = remaining.toVector.filter { f =>
        !remaining.exists(f2 => f != f2 && (f.signature.inputType fits f2.signature.inputType))
      }

      // If the top is empty, the algorithm will result in an endless loop, as no remaining functions are removed
      // during this iteration. Hence, we abort the compilation before that can happen.
      if (top.isEmpty) {
        throw CompilationException(s"The list of top functions is empty, meaning the multi-function hierarchy construction would be stuck in an endless loop. Remaining functions: $remaining.")
      }

      top.foreach { f =>
        // The min set must be calculated before the node is added to the graph.
        val min = Dispatch.min(asHierarchy, f.signature.inputType).map(_.definition)

        graph = graph.incl(f)
        min.foreach { g =>
          graph = graph.incl(g ~> f)
        }

        remaining = remaining.excl(f)
      }
    }

    if (!graph.isAcyclic) {
      throw CompilationException(s"The dispatch hierarchy of ${mf.name} is cyclic and thus invalid.")
    }

    asHierarchy
  }

}
