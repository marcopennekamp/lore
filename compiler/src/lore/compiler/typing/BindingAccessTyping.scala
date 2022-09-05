package lore.compiler.typing

import lore.compiler.core.CompilationException
import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.semantics.bindings.{AmbiguousMultiFunction, StructConstructorBinding, TypedTermBinding, UntypedLocalVariable}
import lore.compiler.semantics.expressions.typed.Expression
import lore.compiler.semantics.expressions.typed.Expression.{BindingAccess, ConstructorValue}
import lore.compiler.semantics.expressions.untyped.UntypedExpression.UntypedBindingAccess
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.types.{FunctionType, Type}

object BindingAccessTyping {

  def checkOrInfer(
    expression: UntypedBindingAccess,
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit reporter: Reporter): Option[Expression] = {
    expression.binding match {
      // Multi-functions which aren't directly used in a simple call must be coerced to function values.
      case mf: MultiFunctionDefinition =>
        MultiFunctionTyping.checkOrInferValue(mf, expression, expectedType)

      case AmbiguousMultiFunction(mfs) =>
        // TODO (multi-import): Multi-function value stuff...
        ???

      case binding: StructConstructorBinding if binding.isConstant =>
        Some(ConstructorValue(binding.underlyingType, expression.position))

      case binding: StructConstructorBinding => expectedType match {
        case Some(expectedType: FunctionType) =>
          CallTyping.inferTypeArguments(binding.signature, expectedType.identity.parameterTypes)(
            TypingFeedback.ConstructorValue.IllegalArity(binding.signature, expectedType, expression),
          ).map { typeArgumentAssignments =>
            ConstructorValue(binding.instantiateStructType(typeArgumentAssignments), expression.position)
          }

        case Some(expectedType) =>
          reporter.report(TypingFeedback.ConstructorValue.FunctionTypeExpected(expression, expectedType))
          None

        case None =>
          reporter.report(TypingFeedback.ConstructorValue.TypeContextExpected(expression))
          None
      }

      case variable: UntypedLocalVariable =>
        // When an untyped local variable is accessed, its corresponding typed local variable must already be contained
        // in the inference context.
        val typedVariable = context.localVariables.getOrElse(
          variable.uniqueKey,
          throw CompilationException(
            s"Cannot find typed local variable ${variable.name} in inference context. Access position:" +
              s" ${expression.position}."
          )
        )
        Some(BindingAccess(typedVariable, expression.position))

      case binding: TypedTermBinding =>
        // Global variables, struct objects, and function parameters (in the form of LocalVariables) don't need to be
        // coerced or inferred.
        Some(BindingAccess(binding, expression.position))
    }
  }

}
