package lore.compiler.semantics.functions

import lore.compiler.types.{Fit, ProductType, Type}

import scala.collection.mutable

object Dispatch {

  /**
    * Calculates the given hierarchy's fit set for the given type.
    */
  def fit(hierarchy: DispatchHierarchy, tpe: Type): Vector[FunctionDefinition] = {
    traverse(hierarchy)(
      // We only have to visit nodes that are a supertype of the input type, because any children of these nodes
      // won't be a supertype of the input type if their parent isn't already a supertype.
      visit  = predicateVisitFit(hierarchy, Type.tupled(tpe)),
      select = _ => true,
    )
  }

  /**
    * Calculates the given hierarchy's min set for the given type.
    */
  def min(hierarchy: DispatchHierarchy, tpe: Type): Vector[FunctionDefinition] = {
    // Even though min is defined in terms of the fit, we don't use the fit function and instead compute everything in
    // one traversal.
    val visit = predicateVisitFit(hierarchy, Type.tupled(tpe)) _
    traverse(hierarchy)(
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
    * Returns the function with the exact given input type contained in the given hierarchy.
    */
  def exact(hierarchy: DispatchHierarchy, tpe: Type): Option[FunctionDefinition] = {
    // TODO: We cannot get fixed functions with type arguments, because an actual type and a type variable could never
    //       be equally specific... How can we deal with this? Obviously, we need to allow getting a fixed
    //       function with type variables if we want a complete programming language. If we do so, we will also have
    //       to ensure that we don't select more than one node, as this is suddenly possible if we do the fit shtick
    //       first.

    // Using traverseHierarchy ensures that we only visit subtrees that could contain the exact candidate.
    val input = Type.tupled(tpe)
    traverse(hierarchy)(
      visit  = predicateVisitFit(hierarchy, input),
      select = node => Fit.isEquallySpecific(input, node.signature.inputType),
    ).headOption
  }

  /**
    * When used as a visit-predicate for [[traverse]], all and only nodes that are part of the function
    * fit for a given input type are visited.
    */
  private def predicateVisitFit(hierarchy: DispatchHierarchy, input: ProductType)(node: hierarchy.graph.NodeT): Boolean = Fit.fits(input, node.signature.inputType)

  /**
    * Traverses the hierarchy, visiting all nodes for which visit(node) is true, selecting all nodes for which
    * visit(node) AND select(node) are true. Each node is visited only once, even if it has more than one
    * incoming edge.
    */
  private def traverse(hierarchy: DispatchHierarchy)(
    visit: hierarchy.graph.NodeT => Boolean,
    select: hierarchy.graph.NodeT => Boolean,
  ): Vector[FunctionDefinition] = {
    var remaining = hierarchy.roots
    var results: Vector[FunctionDefinition] = Vector.empty
    val visited = mutable.HashSet[FunctionDefinition]()
    while (remaining.nonEmpty) {
      val node = remaining.head
      remaining = remaining.tail
      if (!visited.contains(node) && visit(node)) {
        if (select(node)) {
          results = results :+ node
        }

        // Add the children to the unseen list in any case. Whether they should be visited is checked later.
        remaining = node.diSuccessors.toVector ++ remaining
      }
      visited.add(node)
    }
    results
  }

}