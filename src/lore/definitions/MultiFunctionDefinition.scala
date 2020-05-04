package lore.definitions

import lore.types.{ProductType, Type}
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.Graph

import scala.collection.mutable

case class MultiFunctionDefinition(name: String, functions: List[FunctionDefinition]) {
  /**
    * A hierarchy of functions ordered according to their input types. Not necessarily connected, but
    * definitely acyclic.
    */
  private val hierarchy: Graph[FunctionDefinition, DiEdge] = Graph()
  private implicit val edgeFactory = DiEdge
  buildHierarchy()

  // TODO: Instead of building a hierarchy, it might be cheaper to simply compare all functions IF the function
  //       list is small or linear. For example, if there are only 10 functions associated with the multi-function,
  //       building the hierarchy structure and traversal structures for traverseHierarchy might be more expensive
  //       than going through all elements.

  /**
    * All root nodes in the hierarchy, i.e. those functions with a super-function.
    */
  private lazy val hierarchyRoots: List[hierarchy.NodeT] = hierarchy.nodes.filter(_.inDegree == 0).toList

  /**
    * When used as a visit-predicate for [[traverseHierarchy]], all and only nodes that are part of the function
    * fit for a given input type are visited.
    */
  private def visitFit(input: ProductType)(node: hierarchy.NodeT): Boolean = input <= node.signature.inputType

  /**
    * Calculates the multi-function fit.
    */
  def fit(tpe: Type): List[FunctionDefinition] = {
    traverseHierarchy(
      // We only have to visit nodes that are a supertype of the input type, because any children of these nodes
      // won't be a supertype of the input type if their parent isn't already a supertype.
      visit  = visitFit(tpe.toTuple),
      select = _ => true,
    )
  }

  /**
    * Calculates the multi-function min.
    */
  def min(tpe: Type): List[FunctionDefinition] = {
    // Even though min is defined in terms of the fit, we don't use the fit function and instead compute everything in
    // one traversal.
    val visit = visitFit(tpe.toTuple) _
    traverseHierarchy(
      visit,
      // We select all nodes for which no children are visited. This is easy to see: Min is defined in terms of
      // "there are no other fitting functions which have a smaller input type than this". If none of the children
      // are visited, all possible sub-functions of this node don't fit the given input type; this means that the
      // node at hand represents the "end of the chain" in terms of possible functions to call. We have found the
      // minimum possible function in this part of the graph.
      select = node => !node.diSuccessors.exists(visit),
    )
  }

  /**
    * Returns the function with the exact given input type.
    */
  def exact(tpe: Type): Option[FunctionDefinition] = {
    // Using traverseHierarchy ensures that we only visit subtrees that could contain the exact candidate.
    val input = tpe.toTuple
    traverseHierarchy(
      visit  = node => input <= node.signature.inputType,
      select = node => input == node.signature.inputType,
    ).headOption
  }

  /**
    * Traverses the hierarchy, visiting all nodes for which visit(node) is true, selecting all nodes for which
    * visit(node) AND select(node) are true. Each node is visited only once, even if it has more than one
    * incoming edge.
    */
  private def traverseHierarchy(
    visit: hierarchy.NodeT => Boolean,
    select: hierarchy.NodeT => Boolean,
  ): List[FunctionDefinition] = {
    var remaining = hierarchyRoots
    var results: List[FunctionDefinition] = Nil
    val visited = mutable.HashSet[FunctionDefinition]()
    while (remaining.nonEmpty) {
      val node = remaining.head
      remaining = remaining.tail
      if (!visited.contains(node) && visit(node)) {
        if (select(node)) {
          results = node :: results
        }

        // Add the children to the unseen list in any case. Whether they should be visited is checked later.
        remaining = node.diSuccessors.toList ::: remaining
      }
      visited.add(node)
    }
    results
  }

  /**
    * Builds the function hierarchy that is used to define the fit.
    */
  private def buildHierarchy(): Unit = {
    // Algorithm: Sequentially pick all functions which don't have a super-function from the unused set and
    // add them to the graph. For all new functions f, add edges from any 0-out node f2 to f if in(f) < in(f2).
    // A 0-out node is a function which hasn't been associated with any sub-functions yet.
    // The algorithm connects functions whose input types are in direct subtyping relationships in the context of
    // all defined functions within this multi-function. It ensures this property by adding all functions which don't
    // have a super-function to the graph simultaneously. Such functions f must find their super-function g in at
    // least one of the graph's current 0-out nodes, because in the previous iteration, that f specifically wasn't
    // added to the graph because that super-function g was still part of the unused set.
    // Of course, this is not true for the first iteration. Then a function f cannot find a super-function g, but
    // rather becomes a root of the function hierarchy. Note that the graph can always contain functions which are
    // neither associated with sub-functions nor super-functions.
    val unused = mutable.HashSet[FunctionDefinition](functions: _*)
    while (unused.nonEmpty) {
      // All 0-out nodes.
      val zeros = hierarchy.nodes.filter(_.outDegree == 0)

      // All unused functions that don't have a super-function in the unused set.
      val supers = unused.toList.filterNot { f =>
        unused.filter(f2 => f2 != f).exists(f2 => f.signature.inputType <= f2.signature.inputType)
      }

      // Add these functions, then potentially connect them with any 0-out nodes.
      supers.foreach { f =>
        hierarchy.add(f)
        zeros.foreach { g =>
          if (f.signature.inputType <= g.signature.inputType) {
            hierarchy.addEdge(g, f)
          }
        }
      }

      // Finally, remove these functions from the unused set.
      supers.foreach(unused.remove)
    }
    assert(hierarchy.isAcyclic)
  }
}
