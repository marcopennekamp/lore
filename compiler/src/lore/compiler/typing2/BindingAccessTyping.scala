package lore.compiler.typing2

import lore.compiler.core.CompilationException
import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.semantics.bindings.{AmbiguousMultiFunction, StructConstructorBinding, TypedTermBinding, UntypedLocalVariable}
import lore.compiler.semantics.expressions.Expression.{BindingAccess, ConstructorValue}
import lore.compiler.semantics.expressions.untyped.UntypedExpression.UntypedBindingAccess
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.types.{FunctionType, Type}

object BindingAccessTyping {

  def checkOrInfer(
    expression: UntypedBindingAccess,
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit reporter: Reporter): Option[InferenceResult] = {
    expression.binding match {
      // Multi-functions which aren't directly used in a simple call must be coerced to function values.
      case mf: MultiFunctionDefinition =>
        // TODO (multi-import): Multi-function value stuff.

        // From Synthesizer:
//        // We can infer a multi-function value without a function type context if the multi-function has a single,
//        // monomorphic function.
//        mf.functions match {
//          case Vector(function) if function.isMonomorphic =>
//            MultiFunctionValueSynthesizer.handleFunctionInstance(function.monomorphicInstance, expression, None, assignments)
//
//          case _ =>
//            reporter.error(TypingFeedback.MultiFunctionValue.TypeContextExpected(expression))
//            None
//        }

        // From Checker:
        /* expectedType match {
          case expectedType@FunctionType(expectedInput, _) =>
            mf.dispatch(
              expectedInput,
              MultiFunctionFeedback.Dispatch.EmptyFit(mf, expectedInput, position),
              min => MultiFunctionFeedback.Dispatch.AmbiguousCall(mf, expectedInput, min, position),
            ) match {
              case Some(instance) => MultiFunctionValueSynthesizer.handleFunctionInstance(
                instance,
                expression,
                Some(expectedType),
                assignments
              )

              case None =>
                // `dispatch` already reported an error.
                None
            }

          case BasicType.Any =>
            // If the expected type isn't a function type, but still can be a supertype of the function type, the
            // Synthesizer may be able to infer the multi-function value if the multi-function contains a single,
            // monomorphic function.
            fallback

          case _ =>
            reporter.error(TypingFeedback.MultiFunctionValue.FunctionTypeExpected(expression, expectedType))
            None
        } */
        ???

      case AmbiguousMultiFunction(mfs) =>
        // TODO (multi-import): Multi-function value stuff...
        ???

      case binding: StructConstructorBinding if binding.isConstant =>
        Some(ConstructorValue(binding, binding.underlyingType, expression.position), context)

      case binding: StructConstructorBinding => expectedType match {
        case Some(FunctionType(expectedInputType, _)) =>
//          ArgumentSynthesizer2.inferTypeArguments(binding.signature, expectedInputType.elements, context, expression).flatMap {
//            case ArgumentSynthesizer.Result(assignments2, typeArguments) =>
//              InferenceVariable.assign(
//                tpe,
//                binding.instantiateStructType(typeArguments).constructorSignature.functionType,
//                assignments2,
//              )
//          }
          ???

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
        Some(BindingAccess(typedVariable, expression.position), context)

      case binding: TypedTermBinding =>
        // Global variables and struct objects don't need to be coerced or inferred.
        Some(BindingAccess(binding, expression.position), context)
    }
  }

}
