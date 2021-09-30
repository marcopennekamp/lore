package lore.compiler.typing

import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.inference.Inference.Assignments
import lore.compiler.inference.InferenceBounds.BoundType
import lore.compiler.inference.{Inference, InferenceBounds, InferenceVariable}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.{BasicType, Type}

// TODO (inference): This obviously needs a more descriptive name.
object Helpers {
  case class AssignmentFailed(iv: InferenceVariable, tpe: Type, assignments: Assignments, context: Expression) extends Feedback.Error(context) {
    override def message: String = s"The inference variable $iv cannot be assigned the type $tpe given the existing assignment ${InferenceVariable.bounds(iv, assignments)}."
  }

  /**
    * Assigns `tpe` to `iv` in the given assignments.
    *
    * TODO (inference): This uses the old bounds assignment style of [[InferenceBounds.narrowBounds]], but might not
    *                   need the lower/upper bounds split. We will have to see, in the future, how this form of
    *                   assignment can be simplified.
    */
  def assign(iv: InferenceVariable, tpe: Type, assignments: Assignments): Option[Assignments] = {
    val bounds = InferenceVariable.bounds(iv, assignments)

    if (bounds.lower <= tpe && tpe <= bounds.upper) {
      Some(assignments.updated(iv, InferenceBounds(iv, tpe, tpe)))
    } else {
      None
    }
  }

  // TODO (inference): This error needs to be moved, of course, but it's just a sort of temporary error that will allow
  //                   us to see how robust this system is, and then implement targeted solutions.
  case class IncompleteAssignments(tpe: Type, result: Type, assignments: Assignments, context: Expression) extends Feedback.Error(context) {
    override def message: String = s"The type $tpe cannot be instantiated given the current assignments. The result is $result."
  }

  /**
    * Instantiates all inference variables in `tpe` from the given assignments. The function reports an error if not
    * all inference variables can be instantiated.
    *
    * TODO (inference): This should rather return `None` if the type cannot be fully instantiated. This allows the
    *                   caller of `instantiate` to properly handle an instantiation failure.
    */
  def instantiate(tpe: Type, assignments: Assignments, context: Expression)(implicit reporter: Reporter): Type = {
    val result = Inference.instantiate(assignments, tpe, BoundType.Lower, (bounds, _) => {
      if (bounds.lower > BasicType.Nothing) bounds.lower
      else if (bounds.upper < BasicType.Any) bounds.upper
      else bounds.variable // This case is what separates the instantiation from `instantiateCandidateType`.
    })
    if (!Inference.isFullyInstantiated(result)) {
      reporter.error(IncompleteAssignments(tpe, result, assignments, context))
    }
    result
  }
}
