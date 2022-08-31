package lore.compiler.typing2

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.typed.Expression.ConstructorCall
import lore.compiler.semantics.expressions.untyped.UntypedExpression.UntypedConstructorCall
import lore.compiler.types.DeclaredType

object ConstructorTyping {

  // TODO (multi-import): Document.
  def checkOrInferCall(
    expression: UntypedConstructorCall,
    expectedType: Option[DeclaredType],
    context: InferenceContext,
  )(implicit checker: Checker2, registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    // TODO (multi-import): Do we need to incorporate this? I don't think so, because this is already handled by
    //                      unifying the constructor's output type with the expected type, which has been added with
    //                      the new typing approach.
    // Given that we have an expected struct type, we must unify the expected constructor's parameters with the
    // actual parameter types so that any inference variables already known can be preassigned. For example, let's
    // say we have an expected struct type `Wrapper[Int, Any]` from the test case `language/inference/wrapper.lore`.
    // That is, we know the input type of the wrapper's function property. The expected constructor parameter is thus
    // typed as `Int => Any`. The unification takes care that the inference variable for `A` is assigned `Int` as an
    // upper bound.
//    val expectedParameterTypes = expectedStructType.constructorSignature.parameters.map(_.tpe)
//    val assignments2 = Unification.unifySubtypes(parameterTypes, expectedParameterTypes, assignments).getOrElse {
//      return None
//    }

    Typing2.traceCheckOrInfer("constructor call", expression, expectedType)
    Typing2.indentationLogger.indented {
      val binding = expression.target
      CallTyping.checkOrInfer(binding.signature, expression, expectedType, context) {
        case (typedArguments, assignments) =>
          ConstructorCall(binding.instantiateStructType(assignments), typedArguments, expression.position)
      }
    }
  }

}
