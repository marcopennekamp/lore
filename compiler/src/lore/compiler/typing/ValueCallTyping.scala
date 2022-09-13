package lore.compiler.typing

import lore.compiler.core.Positioned
import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.typed.Expression
import lore.compiler.semantics.expressions.typed.Expression.ValueCall
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.types.FunctionType
import lore.compiler.typing.CallTyping.UntypedOrTypedExpression

object ValueCallTyping {

  /**
    * Infers a value call from `target` and `arguments`. `arguments` can individually be untyped or already typed.
    * Untyped arguments will be checked with the expected argument type, while typed arguments must adhere to the
    * expected argument type.
    *
    * To support uniform call syntax typing, [[infer]] does not directly expect an
    * [[lore.compiler.semantics.expressions.untyped.UntypedExpression.UntypedValueCall]]. The function that handles UCS
    * redirects is [[UniformCallSyntaxTyping.checkOrInferValueCall]].
    */
  def infer(
    target: UntypedExpression,
    arguments: Vector[UntypedOrTypedExpression],
    positioned: Positioned,
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    Synthesizer.infer(target, context).flatMap {
      case (typedTarget, context2) => infer(typedTarget, arguments, positioned, context2)
    }
  }

  /**
    * Infers a value call with an already typed `target`. See the other [[infer]].
    */
  def infer(
    target: Expression,
    arguments: Vector[UntypedOrTypedExpression],
    positioned: Positioned,
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    target.tpe match {
      case targetType: FunctionType =>
        CallTyping.checkOrInfer(targetType.identity, arguments, Some(targetType.output), positioned, context)(
          (typedArguments, _) => ValueCall(target, typedArguments, targetType.output, positioned.position)
        )

      case targetType =>
        reporter.error(TypingFeedback.ValueCall.FunctionExpected(targetType, positioned))
        None
    }
  }

}
