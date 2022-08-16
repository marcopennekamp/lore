package lore.compiler.typing2

import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.semantics.bindings.LocalVariable
import lore.compiler.semantics.expressions.Expression.{LambdaParameter, LambdaValue}
import lore.compiler.semantics.expressions.untyped.UntypedExpression.{UntypedLambdaParameter, UntypedLambdaValue}
import lore.compiler.types.{FunctionType, Type}
import lore.compiler.utils.CollectionExtensions.Tuple2OptionExtension

object LambdaTyping {

  /**
    * Checks a lambda value for which an `expectedType` is available.
    *
    * Even if the lambda value is fully annotated, this function is useful because it uses the expected function type's
    * output type to check the lambda body. If the lambda value is fully annotated, but the expected type is not a
    * function type, [[check]] delegates to [[Synthesizer2.infer]]. (Such a delegation will give the compiler the
    * chance to infer the lambda without a context function type. Even if the context type is, say, a trait type `T`,
    * we want the compiler to infer the best type `X => Y` for the lambda, and <i>then</i> report that function type
    * `X => Y` is not a subtype of expected type `T`.)
    */
  def check(
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
        val parameterTypes = expression.parameters.zip(expectedInputType.elements).map {
          case (parameter, expectedParameterType) => parameter.typeAnnotation match {
            case Some(parameterType) =>
              // The function input is contravariant, so we have to check that the expected type is a subtype of the
              // actual type.
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
        }

        val (typedParameters, context2) = buildTypedParameters(expression.parameters, parameterTypes, context)
        checker.check(expression.body, expectedOutputType, context2).mapFirst { typedBody =>
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
        if (expression.isFullyAnnotated) {
          Synthesizer2.infer(expression, context)
        } else {
          reporter.error(TypingFeedback.AnonymousFunction.FunctionTypeExpected2(expression, expectedType))
          None
        }
    }
  }

  /**
    * Builds [[LambdaParameter]]s from `parameters` and `parameterTypes`, adding the requisite local variables to
    * `context`. This function does NOT check whether the given types and the annotated types (if any) agree.
    */
  def buildTypedParameters(
    parameters: Vector[UntypedLambdaParameter],
    parameterTypes: Vector[Type],
    context: InferenceContext,
  ): (Vector[LambdaParameter], InferenceContext) = {
    parameters.zip(parameterTypes).foldLeft((Vector.empty[LambdaParameter], context)) {
      case ((typedParameters, context2), (parameter, tpe)) =>
        val typedVariable = LocalVariable(parameter.variable, tpe)
        (
          typedParameters :+ LambdaParameter(typedVariable, parameter.position),
          context2.withLocalVariable(typedVariable),
        )
    }
  }

}
