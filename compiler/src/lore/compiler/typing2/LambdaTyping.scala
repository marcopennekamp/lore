package lore.compiler.typing2

import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.semantics.expressions.Expression.{LambdaValue, LambdaParameter}
import lore.compiler.semantics.expressions.untyped.UntypedExpression.UntypedLambdaValue
import lore.compiler.types.{FunctionType, Type}
import lore.compiler.utils.CollectionExtensions.Tuple2OptionExtension

object LambdaTyping {

  /**
    * Checks a lambda value which isn't fully annotated.
    */
  def checkVagueLambdaValue(
    expression: UntypedLambdaValue,
    expectedType: Type,
    context: InferenceContext,
  )(implicit checker: Checker2, reporter: Reporter): Option[InferenceResult] = {
    // If a lambda function is missing a parameter type declaration, it requires the expected type to be a
    // function type.
    expectedType match {
      case FunctionType(expectedInputType, expectedOutputType)
        if expectedInputType.elements.length == expression.parameters.length
      =>
        val typedParameters = expression.parameters.zip(expectedInputType.elements).map {
          case (parameter, expectedParameterType) =>
            val parameterType = parameter.typeAnnotation match {
              case Some(parameterType) =>
                // Function input is contravariant, so we have to check that the expected type is a subtype of
                // the actual type.
                if (expectedParameterType <= parameterType) {
                  parameterType
                } else {
                  reporter.error(
                    TypingFeedback.AnonymousFunction.IllegalParameterType(
                      expectedParameterType,
                      parameterType,
                      parameter.position,
                    )
                  )
                  return None
                }

              case None => expectedParameterType
            }

            // TODO (multi-import): Some parts of the AnonymousFunctionParameter construction should be moved to a
            //                      helper function which the synthesizer can use.
            LambdaParameter(
              parameter.uniqueKey,
              parameter.name,
              parameterType,
              parameter.position,
            )
        }

        checker.check(expression.body, expectedOutputType, context).mapFirst { typedBody =>
          LambdaValue(
            typedParameters,
            typedBody,
            expression.position,
          )
        }

      case expectedType: FunctionType =>
        reporter.error(TypingFeedback.AnonymousFunction.IllegalArity2(expression, expectedType))
        None

      case _ =>
        reporter.error(TypingFeedback.AnonymousFunction.FunctionTypeExpected2(expression, expectedType))
        None
    }
  }

}
