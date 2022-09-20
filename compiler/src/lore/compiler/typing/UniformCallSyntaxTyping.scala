package lore.compiler.typing

import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.bindings.{AmbiguousMultiFunction, TypedTermBinding, UntypedLocalVariable}
import lore.compiler.semantics.expressions.typed.Expression.MemberAccess
import lore.compiler.semantics.expressions.untyped.UntypedExpression.{UntypedBindingAccess, UntypedLambdaValue, UntypedMemberAccess, UntypedValueCall}
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.types.Type
import lore.compiler.typing.CallTyping.UntypedOrTypedExpression
import lore.compiler.utils.CollectionExtensions.Tuple2OptionExtension

object UniformCallSyntaxTyping {

  /**
    * Checks or infers `expression`, either producing a member access or a function call (uniform call syntax). If the
    * member to be accessed is neither a member nor a callable function, a "member not found" error will be reported.
    * If uniform call syntax is applicable, but the instance isn't a legal argument, function call errors will be
    * reported instead.
    *
    * If `valueCall` is provided, its arguments will be appended to the existing argument in uniform call syntax. If
    * `expression` is not a UCS call, `valueCall` will instead be used to infer a value call expression with the member
    * access as its call target.
    */
  def checkOrInfer(
    expression: UntypedMemberAccess,
    valueCall: Option[UntypedValueCall],
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    // Special case: Function values usually need a type context to be inferred, which means the instance cannot be
    // inferred. Because function values don't have any members, the algorithm can just assume this member access to be
    // a UCS call. Also see "Uniform Call Syntax" in the specification.
    val isImmediateUcs = !Inferability.isDefinitelyInferable(expression.instance) && (expression.instance match {
      case _: UntypedLambdaValue => true
      case UntypedBindingAccess(_: MultiFunctionDefinition, _) => true
      case UntypedBindingAccess(AmbiguousMultiFunction(_), _) => true
      case _ => false
    })

    if (isImmediateUcs) {
      return handleUcs(expression, valueCall, expectedType, Left(expression.instance), context)
    }

    Synthesizer.infer(expression.instance, context).flatMap { case (typedInstance, context2) =>
      typedInstance.tpe.member(expression.name) match {
        case Some(member) =>
          val memberAccess = MemberAccess(typedInstance, member, expression.position)
          valueCall match {
            case Some(valueCall) =>
              ValueCallTyping.infer(memberAccess, valueCall.arguments.map(Left(_)), valueCall, context2)

            case None => Some(memberAccess, context2)
          }

        case None => handleUcs(expression, valueCall, expectedType, Right(typedInstance), context2)
      }
    }
  }

  private def handleUcs(
    expression: UntypedMemberAccess,
    valueCall: Option[UntypedValueCall],
    expectedType: Option[Type],
    instance: UntypedOrTypedExpression,
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    lazy val callPositioned = valueCall.getOrElse(expression)

    def memberNotFound = {
      reporter.report(TypingFeedback.Member.NotFound(expression, instance))
      None
    }

    def getArguments = {
      CallTyping.inferArguments(valueCall.map(_.arguments).getOrElse(Vector.empty), context).mapFirst {
        arguments => instance +: arguments
      }
    }

    def handleValueCallTarget(binding: TypedTermBinding) = {
      if (binding.tpe.isFunction) {
        getArguments.flatMap { case (arguments, context2) =>
          ValueCallTyping.infer(
            UntypedBindingAccess(binding, expression.position),
            arguments,
            callPositioned,
            context2,
          )
        }
      } else {
        // If the binding is not a function, instead of reporting a "function expected" error and confusing the user,
        // the compiler should report a "member not found" error. This is consistent with UCS only being applicable to
        // function UCS bindings.
        memberNotFound
      }
    }

    // If UCS applies, we need to make sure that `instance` isn't inferred a second time. Hence, the functions
    // that check/infer the resulting calls accept prepared arguments.
    expression.ucsBinding match {
      case Some(mf: MultiFunctionDefinition) => getArguments.flatMap { case (arguments, context2) =>
        MultiFunctionTyping.checkOrInferCall(mf, arguments, expectedType, callPositioned, context2)
      }

      case Some(AmbiguousMultiFunction(mfs)) => getArguments.flatMap { case (arguments, context2) =>
        MultiFunctionTyping.checkOrInferAmbiguousCall(mfs, arguments, expectedType, callPositioned, context2)
      }

      case Some(binding: TypedTermBinding) => handleValueCallTarget(binding)

      case Some(binding: UntypedLocalVariable) => context.localVariables.get(binding.uniqueKey) match {
        case Some(localVariable) => handleValueCallTarget(localVariable)
        case None => memberNotFound
      }

      case _ => memberNotFound
    }
  }

  /**
    * Checks or infers a value call, either inferring it at face-value, or checking/inferring it as a UCS call if the
    * target is an applicable member access expression.
    */
  def checkOrInferValueCall(
    expression: UntypedValueCall,
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    expression.target match {
      case access: UntypedMemberAccess => checkOrInfer(access, Some(expression), expectedType, context)
      case _ => ValueCallTyping.infer(expression.target, expression.arguments.map(Left(_)), expression, context)
    }
  }

}
