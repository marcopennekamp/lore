package lore.compiler.typing

import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.{BasicType, Type}
import lore.compiler.typing.InferenceBounds.BoundType
import lore.compiler.typing.InferenceVariable.Assignments

// TODO (inference): This obviously needs a more descriptive name.
object Helpers {
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
    val result = InferenceVariable.instantiate(assignments, tpe, BoundType.Lower, (bounds, _) => {
      if (bounds.lower > BasicType.Nothing) bounds.lower
      else if (bounds.upper < BasicType.Any) bounds.upper
      else bounds.variable // This case is what separates the instantiation from `instantiateCandidateType`.
    })
    if (!InferenceVariable.isFullyInstantiated(result)) {
      reporter.error(IncompleteAssignments(tpe, result, assignments, context))
    }
    result
  }

  /**
    * Instantiates the type of `expression` with all inference variables from the given assignments. The function
    * reports an error if not all inference variables can be instantiated.
    */
  def instantiate(expression: Expression, assignments: Assignments)(implicit reporter: Reporter): Type = instantiate(expression.tpe, assignments, expression)

  def instantiate(expressions: Vector[Expression], assignments: Assignments)(implicit reporter: Reporter): Vector[Type] = expressions.map(instantiate(_, assignments))

  /**
    * Guesses the best instantiation for `tpe` from the given assignments. This can be used to preprocess types for
    * error reporting.
    */
  def instantiateCandidate(tpe: Type, assignments: Assignments): Type = InferenceVariable.instantiateCandidateType(assignments, tpe)

  def traceExpressionType(expression: Expression, assignments: Assignments, label: String, additional: String = ""): Unit = {
    Typing.logger.whenTraceEnabled {
      val inferredType = Helpers.instantiateCandidate(expression.tpe, assignments)
      Typing.logger.trace(s"$label type $inferredType for `${expression.position.truncatedCode}`.$additional")
    }
  }
}
