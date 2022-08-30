package lore.compiler.typing2

import lore.compiler.feedback.{Feedback, Reporter, TypingFeedback}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.semantics.expressions.untyped.UntypedExpression.UntypedCall
import lore.compiler.semantics.functions.{FunctionLike, FunctionSignature}
import lore.compiler.types.{Fit, TupleType, Type, TypeVariable}
import lore.compiler.typing2.unification.InferenceBounds2.BoundType2
import lore.compiler.typing2.unification.{InferenceAssignments, InferenceVariable2, Unification2}
import lore.compiler.utils.CollectionExtensions.{Tuple2OptionExtension, VectorExtension}

object CallTyping {

  /**
    * TODO (multi-import): Document.
    * TODO (multi-import): Note that `checkOrInfer` does tracing, but the caller will have to ensure there's a trace
    *                      "heading" and an indent, like ConstructorTyping does.
    */
  def checkOrInfer(
    function: FunctionLike,
    expression: UntypedCall,
    expectedType: Option[Type],
    context: InferenceContext,
  )(
    buildCallExpression: (Vector[Expression], TypeVariable.Assignments) => Expression,
  )(implicit checker: Checker2, registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    val (inferredArguments, context2) = inferArguments(expression, context)
    checkOrInfer(function, expression, inferredArguments, expectedType, context2)(buildCallExpression)
  }

  /**
    * Checks or infers a call expression, like [[checkOrInfer]], but with the distinction that arguments must have been
    * pre-inferred with [[inferArguments]]. This variant is intended to be used with multi-function call inference,
    * which often has to check many function definitions, but arguments only need to be inferred once.
    *
    * `toResult` builds an arbitrary result `R` instead of an [[Expression]] because multi-function call checking
    * builds an expression at a later stage.
    */
  def checkOrInfer[R](
    function: FunctionLike,
    expression: UntypedCall,
    inferredArguments: Vector[Option[Expression]],
    expectedType: Option[Type],
    context: InferenceContext,
  )(
    toResult: (Vector[Expression], TypeVariable.Assignments) => R,
  )(implicit checker: Checker2, reporter: Reporter): Option[(R, InferenceContext)] = {
    if (expression.arity != function.arity) {
      reporter.error(TypingFeedback.Call.IllegalArity(expression.arity, function.arity, expression))
      return None
    }

    if (function.isMonomorphic) {
      return inferMonomorphic(function, expression, context)(toResult)
    }

    // (1) Replace type variables with inference variables in the function's input and output types.
    val (parameterTypes, tvToIv) = InferenceVariable2.fromTypeVariables(function.parameterTypes, function.typeParameters)
    val outputType = Type.substitute(function.outputType, tvToIv)
    val typeParameters = function.typeParameters.map(tvToIv)

    // (2) Unify inferred arguments with their respective parameter types to take a first stab at assigning type
    //     arguments. This also ensures that each inferred argument actually fits into its parameter type.
    val assignments = inferredArguments
      .zip(parameterTypes)
      .flatMap {
        case (Some(argument), parameterType) => Some(argument, parameterType)
        case _ => None
      }
      .foldSome(Map.empty: InferenceAssignments) { case (assignments2, (argument, parameterType)) =>
        unifyArgumentType(argument, parameterType, typeParameters, assignments2)
      }
      .getOrElse(return None)

    // (3) If we have an expected output type, we can unify this type with the function's output type to potentially
    //     assign additional type arguments.
    val assignments2 = expectedType match {
      case Some(expectedType) =>
        Unification2.unifySubtypes(outputType, expectedType, assignments).getOrElse(assignments)
      case _ => assignments
    }

    // (4) `check` the untyped arguments from left to right. In the process, we have to attempt unification for each
    //     newly typed argument to further narrow down the type arguments, and also do bounds processing to allow
    //     changes in inference variable assignments to carry across bounds. The unification also ensures that each
    //     argument type fits the parameter type, which until this point had only been established for inferred
    //     arguments.
    val (typedArguments, (assignments3, context2)) = inferredArguments
      .zip(expression.arguments)
      .zip(parameterTypes)
      .foldSomeCollect((assignments2, context)) {
        case ((assignments3, context2), ((None, argument), parameterType)) =>
          checkArgument(argument, parameterType, typeParameters, assignments3, context2)
        case (result, ((Some(typedArgument), _), _)) => Some(typedArgument, result)
      }
      .getOrElse(return None)

    // (5) Build the result expression.
    val typeVariableAssignments = tvToIv.map {
      case (tv, iv) => tv -> InferenceVariable2.instantiateCandidate(iv, assignments3)
    }
    Some(toResult(typedArguments, typeVariableAssignments), context2)
  }

  /**
    * Attempts to infer the call's arguments without a type context. The resulting list contains an inferred expression
    * for each argument, or `None` if the argument couldn't be inferred.
    */
  def inferArguments(expression: UntypedCall, context: InferenceContext)(
    implicit checker: Checker2,
    registry: Registry,
  ): (Vector[Option[Expression]], InferenceContext) = {
    expression.arguments.foldLeft((Vector.empty[Option[Expression]], context)) {
      case ((typedArguments, context2), argument) =>
        Synthesizer2.attempt(argument, context2)._1 match {
          case Some((typedArgument, context3)) => (typedArguments :+ Some(typedArgument), context3)
          case None => (typedArguments :+ None, context2)
        }
    }
  }

  /**
    * An optimized version of [[checkOrInfer]] in case the signature is monomorphic. Inferring the call of a function
    * that doesn't contain any type parameters is much easier and thereby faster. [[inferMonomorphic]] doesn't check
    * for arity as [[checkOrInfer]] must already have handled this.
    */
  private def inferMonomorphic[R](
    function: FunctionLike,
    expression: UntypedExpression.UntypedCall,
    context: InferenceContext,
  )(
    toResult: (Vector[Expression], TypeVariable.Assignments) => R,
  )(implicit checker: Checker2, reporter: Reporter): Option[(R, InferenceContext)] = {
    if (expression.arity != function.arity) {
      reporter.error(TypingFeedback.Function.IllegalArity(expression.arity, function.arity, expression))
      return None
    }

    expression.arguments.zip(function.parameterTypes)
      .foldSomeCollect(context) {
        case (context, (argument, parameterType)) => checker.check(argument, parameterType, context)
      }
      .mapFirst(typedArguments => toResult(typedArguments, Map.empty))
  }

  /**
    * Checks `argument` with `parameterType`. If the argument has the wrong type, an error is reported.
    */
  private def checkArgument(
    argument: UntypedExpression,
    parameterType: Type,
    typeParameters: Vector[InferenceVariable2],
    assignments: InferenceAssignments,
    context: InferenceContext,
  )(
    implicit checker: Checker2,
    reporter: Reporter,
  ): Option[(Expression, (InferenceAssignments, InferenceContext))] = {
    val parameterTypeCandidate = InferenceVariable2.instantiateCandidate(parameterType, assignments)
    Typing2.logger.trace(s"Check untyped argument `${argument.position.truncatedCode}` with parameter type" +
      s" `$parameterTypeCandidate`:")

    Typing2.indentationLogger.indented {
      // Note that `check` reports a subtyping error if the argument's type isn't legal.
      checker.check(argument, parameterTypeCandidate, context).flatMap { case (typedArgument, context2) =>
        // The `check` already ensures that the argument type is a subtype of the parameter type. Hence, argument type
        // unification only makes sense when type parameters are present, as it otherwise serves no purpose.
        if (typeParameters.nonEmpty) {
          unifyArgumentType(typedArgument, parameterType, typeParameters, assignments)
            .map(assignments2 => (typedArgument, (assignments2, context2)))
        } else Some(typedArgument, (assignments, context2))
      }
    }
  }

  /**
    * Unifies the type of `argument` with `parameterType`, possibly assigning type arguments to type parameters, and
    * then unifies the new type arguments with the bounds of each type parameter.
    *
    * This unification has three purposes:
    *   1. It checks that the argument type fits the parameter type.
    *   2. It assigns type arguments to type parameters contained in the parameter type.
    *   3. It checks that narrowed type arguments fit the bounds of their respective type parameters.
    */
  private def unifyArgumentType(
    argument: Expression,
    parameterType: Type,
    typeParameters: Vector[InferenceVariable2],
    assignments: InferenceAssignments,
  )(implicit reporter: Reporter): Option[InferenceAssignments] = {
    val assignments2 = Unification2.unifyFits(argument.tpe, parameterType, assignments).getOrElse {
      reporter.error(
        TypingFeedback.SubtypeExpected(
          argument.tpe,
          InferenceVariable2.instantiateCandidate(parameterType, assignments),
          argument.position,
        )
      )
      return None
    }

    typeParameters.foldSome(assignments2) { case (assignments3, iv) =>
      Unification2.unifyInferenceVariableBounds(iv, assignments3).orElse {
        reporter.error(
          TypingFeedback.IllegalBounds(
            InferenceVariable2.instantiateCandidate(iv, assignments3),
            iv.name,
            InferenceVariable2.instantiateByBound(iv.lowerBound, BoundType2.Lower, assignments3),
            InferenceVariable2.instantiateByBound(iv.upperBound, BoundType2.Upper, assignments3),
            argument.position,
          )
        )
        return None
      }
    }
  }

  /**
    * Infers the type arguments of `function` given the actual argument types. If the argument types don't fit the
    * function's parameter types, `inferTypeArguments` reports an appropriate error.
    */
  def inferTypeArguments(
    function: FunctionLike,
    argumentTypes: Vector[Type],
  )(illegalArity: => Feedback.Error)(implicit reporter: Reporter): Option[TypeVariable.Assignments] = {
    if (argumentTypes.length != function.arity) {
      reporter.error(illegalArity)
      return None
    }

    Fit.fitsAssignments(TupleType(argumentTypes), function.inputType)
  }

}
