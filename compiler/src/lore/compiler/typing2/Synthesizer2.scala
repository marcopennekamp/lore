package lore.compiler.typing2

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.utils.CollectionExtensions.VectorExtension

object Synthesizer2 {

  def infer(
    expression: UntypedExpression,
    context: InferenceContext,
  )(implicit checker: Checker2, reporter: Reporter): Option[InferenceResult] = {
    ???
  }

  /**
    * Executes [[infer]] for all `expressions` and returns a result if typing has succeeded for all expressions.
    */
  def infer(
    expressions: Vector[UntypedExpression],
    context: InferenceContext,
  )(implicit checker: Checker2, reporter: Reporter): Option[InferenceResults] = {
    expressions.foldSome((Vector.empty[Expression], context)) {
      case ((typedExpressions, context), expression) =>
        infer(expression, context).map {
          case (typedExpression, context2) => (typedExpressions :+ typedExpression, context2)
        }
    }
  }

}
