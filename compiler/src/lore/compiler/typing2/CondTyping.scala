package lore.compiler.typing2

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.expressions.Expression.Cond
import lore.compiler.semantics.expressions.untyped.UntypedExpression.UntypedCond
import lore.compiler.types.{BasicType, Type}
import lore.compiler.utils.CollectionExtensions.Tuple2OptionExtension

object CondTyping {

  def checkOrInfer(
    expression: UntypedCond,
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit checker: Checker2, reporter: Reporter): Option[InferenceResult] = {
    checker.check(
      expression.cases.map(_.condition),
      BasicType.Boolean,
      context,
    ).flatMap { case (typedConditions, context2) =>
      checker.checkOrInfer(
        expression.cases.map(_.body),
        expectedType,
        context2,
      ).mapFirst { typedBodies =>
        Cond(typedConditions, typedBodies, expression.position)
      }
    }
  }

}
