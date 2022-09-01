package lore.compiler.typing2

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.typed.Expression.Cond
import lore.compiler.semantics.expressions.untyped.UntypedExpression.UntypedCond
import lore.compiler.types.{BasicType, Type}
import lore.compiler.utils.CollectionExtensions.Tuple2OptionExtension

object CondTyping {

  def checkOrInfer(
    expression: UntypedCond,
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    Checker2.check(
      expression.cases.map(_.condition),
      BasicType.Boolean,
      context,
    ).flatMap { case (typedConditions, context2) =>
      Checker2.checkOrInfer(
        expression.cases.map(_.body),
        expectedType,
        context2,
      ).mapFirst { typedBodies =>
        Cond(typedConditions, typedBodies, expression.position)
      }
    }
  }

}
