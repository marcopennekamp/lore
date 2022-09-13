package lore.compiler.typing

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.typed.Expression.ConstructorCall
import lore.compiler.semantics.expressions.untyped.UntypedExpression.UntypedConstructorCall
import lore.compiler.types.DeclaredType

object ConstructorTyping {

  /**
    * Checks or infers a constructor call. `expectedType` may be used by the typing algorithm to assign type arguments
    * to the constructor's type parameters.
    */
  def checkOrInferCall(
    expression: UntypedConstructorCall,
    expectedType: Option[DeclaredType],
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    Typing.traceCheckOrInfer("constructor call", expectedType, expression)
    Typing.indentationLogger.indented {
      val binding = expression.target
      CallTyping.checkOrInfer(binding.signature, expression, expectedType, context) {
        case (typedArguments, assignments) =>
          ConstructorCall(binding.instantiateStructType(assignments), typedArguments, expression.position)
      }
    }
  }

}
