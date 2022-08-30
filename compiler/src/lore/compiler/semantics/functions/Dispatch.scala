package lore.compiler.semantics.functions

import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.types.TupleType

import scala.collection.mutable

object Dispatch {

  /**
    * Resolves a multiple dispatch application for the given hierarchy and input type. The empty fit and ambiguous call
    * errors must be customized.
    */
  def resolve(
    hierarchy: DispatchHierarchy,
    inputType: TupleType,
    emptyFit: => Feedback.Error,
    ambiguousCall: Vector[FunctionDefinition] => Feedback.Error,
  )(implicit reporter: Reporter): Option[FunctionInstance] = {
    Dispatch.min(hierarchy, inputType) match {
      case Vector(instance) => Some(instance)
      case min =>
        if (min.isEmpty) reporter.error(emptyFit)
        else reporter.error(ambiguousCall(min.map(_.definition)))
        None
    }
  }

  /**
    * Calculates the given hierarchy's fit set for the given type.
    */
  def fit(hierarchy: DispatchHierarchy, tpe: TupleType): Vector[FunctionInstance] = {
    traverse(hierarchy)(
      // We only have to visit nodes that are a supertype of the input type, because any children of these nodes
      // won't be a supertype of the input type if their parent isn't already a supertype.
      visit  = _.instantiate(tpe),
      select = _ => true,
    )
  }

  /**
    * Calculates the given hierarchy's min set for the given type.
    */
  def min(hierarchy: DispatchHierarchy, tpe: TupleType): Vector[FunctionInstance] = {
    // Even though min is defined in terms of the fit, we don't use the fit function and instead compute everything in
    // one traversal.
    traverse(hierarchy)(
      visit = _.instantiate(tpe),
      // We select all nodes for which no children are visited. This is easy to see: Min is defined in terms of
      // "there are no other fitting functions which have a smaller input type than this". If none of the children
      // are visited, all possible sub-functions of this node don't fit the given input type; this means that the
      // node at hand represents the "end of the chain" in terms of possible functions to call. We have found the
      // minimum possible function in this part of the graph.
      // TODO: The fit is calculated at least twice for min-set functions: Once in the `select` and then when `visit`
      //       is reached to get the function instance. We can definitely improve this with a specialized algorithm.
      select = node => !node.diSuccessors.exists(successor => tpe fits successor.signature.inputType),
    )
  }

  /**
    * Traverses the hierarchy, visiting all nodes for which visit(node) is true, selecting all nodes for which
    * visit(node) AND select(node) are true. Each node is visited only once, even if it has more than one
    * incoming edge.
    */
  private def traverse(hierarchy: DispatchHierarchy)(
    visit: hierarchy.graph.NodeT => Option[FunctionInstance],
    select: hierarchy.graph.NodeT => Boolean,
  ): Vector[FunctionInstance] = {
    var remaining = hierarchy.roots
    var results: Vector[FunctionInstance] = Vector.empty
    val visited = mutable.HashSet[FunctionDefinition]()
    while (remaining.nonEmpty) {
      val node = remaining.head
      remaining = remaining.tail
      if (!visited.contains(node)) {
        visit(node).foreach { instance =>
          if (select(node)) {
            results = results :+ instance
          }

          // Add the children to the unseen list in any case. Whether they should be visited is checked later.
          remaining = node.diSuccessors.toVector ++ remaining
        }
      }
      visited.add(node)
    }
    results
  }

}
