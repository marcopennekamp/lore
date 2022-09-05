package lore.compiler.typing

import lore.compiler.feedback.{Feedback, Reporter, TypingFeedback}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.typed.Expression
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.semantics.expressions.untyped.UntypedExpression.UntypedCall
import lore.compiler.semantics.functions.FunctionIdentity
import lore.compiler.types.{Fit, TupleType, Type, TypeVariable}
import lore.compiler.typing.unification.InferenceBounds.BoundType
import lore.compiler.typing.unification.{InferenceAssignments, InferenceVariable, Unification}
import lore.compiler.utils.CollectionExtensions.{Tuple2OptionExtension, VectorExtension}

object CallTyping {

  /**
    * Checks or infers a call expression and builds a resulting call expression with `buildCallExpression`.
    *
    * [[checkOrInfer]] performs trace logging, but doesn't add a preceding heading, nor indentation. See
    * [[ConstructorTyping.checkOrInferCall]] on how to provide context to call typing logs.
    */
  def checkOrInfer(
    function: FunctionIdentity,
    expression: UntypedCall,
    expectedType: Option[Type],
    context: InferenceContext,
  )(
    buildCallExpression: (Vector[Expression], TypeVariable.Assignments) => Expression,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
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
    function: FunctionIdentity,
    expression: UntypedCall,
    inferredArguments: Vector[Option[Expression]],
    expectedType: Option[Type],
    context: InferenceContext,
  )(
    toResult: (Vector[Expression], TypeVariable.Assignments) => R,
  )(implicit registry: Registry, reporter: Reporter): Option[(R, InferenceContext)] = {
    if (expression.arity != function.arity) {
      reporter.error(TypingFeedback.Call.IllegalArity(expression.arity, function.arity, expression))
      return None
    }

    if (function.isMonomorphic) {
      return inferMonomorphic(function, expression, context)(toResult)
    }

    // (1) Replace type variables with inference variables in the function's input and output types.
    val (parameterTypes, tvToIv) = InferenceVariable.fromTypeVariables(function.parameterTypes, function.typeParameters)
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
        Unification.unifySubtypes(outputType, expectedType, assignments).getOrElse(assignments)
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
    Some(toResult(typedArguments, InferenceVariable.toTypeVariableAssignments(tvToIv, assignments3)), context2)
  }

  /**
    * Attempts to infer the call's arguments without a type context. The resulting list contains an inferred expression
    * for each argument, or `None` if the argument couldn't be inferred.
    *
    * Only definitely inferable arguments are inferred by this function. Other arguments require type information from
    * the call target and cannot be inferred immediately.
    */
  def inferArguments(
    expression: UntypedCall,
    context: InferenceContext,
  )(implicit registry: Registry): (Vector[Option[Expression]], InferenceContext) = {
    // TODO (multi-import): If an argument is definitely inferable and the attempt results in a `None`, report that as
    //                      an error right away and don't continue with the call typing after that.
    val result = expression.arguments.foldLeft((Vector.empty[Option[Expression]], context)) {
      case ((typedArguments, context2), argument) if Inferability.isDefinitelyInferable(argument) =>
        Synthesizer.attempt(argument, context2)._1 match {
          case Some((typedArgument, context3)) => (typedArguments :+ Some(typedArgument), context3)
          case None => (typedArguments :+ None, context2)
        }
      case ((typedArguments, context2), _) => (typedArguments :+ None, context2)
    }
    Typing.logger.trace(s"Pre-inferred argument types:" +
      s" (${result._1.map(_.map(_.tpe.toString).getOrElse("None")).mkString(", ")}).")
    result
  }

  /**
    * An optimized version of [[checkOrInfer]] in case the signature is monomorphic. Inferring the call of a function
    * that doesn't contain any type parameters is much easier and thereby faster. [[inferMonomorphic]] doesn't check
    * for arity as [[checkOrInfer]] must already have handled this.
    */
  private def inferMonomorphic[R](
    function: FunctionIdentity,
    expression: UntypedExpression.UntypedCall,
    context: InferenceContext,
  )(
    toResult: (Vector[Expression], TypeVariable.Assignments) => R,
  )(implicit registry: Registry, reporter: Reporter): Option[(R, InferenceContext)] = {
    expression.arguments.zip(function.parameterTypes)
      .foldSomeCollect(context) {
        case (context, (argument, parameterType)) => Checker.check(argument, parameterType, context)
      }
      .mapFirst(typedArguments => toResult(typedArguments, Map.empty))
  }

  /**
    * Checks `argument` with `parameterType`. If the argument has the wrong type, an error is reported.
    */
  private def checkArgument(
    argument: UntypedExpression,
    parameterType: Type,
    typeParameters: Vector[InferenceVariable],
    assignments: InferenceAssignments,
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[(Expression, (InferenceAssignments, InferenceContext))] = {
    val parameterTypeCandidate = InferenceVariable.instantiateCandidate(parameterType, assignments)
    Typing.logger.trace(s"Check untyped argument `${argument.position.truncatedCode}` with parameter type" +
      s" `$parameterTypeCandidate`:")

    Typing.indentationLogger.indented {
      // Note that `check` reports a subtyping error if the argument's type isn't legal.
      Checker.check(argument, parameterTypeCandidate, context).flatMap { case (typedArgument, context2) =>
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
    typeParameters: Vector[InferenceVariable],
    assignments: InferenceAssignments,
  )(implicit reporter: Reporter): Option[InferenceAssignments] = {
    val assignments2 = Unification.unifyFits(argument.tpe, parameterType, assignments).getOrElse {
      reporter.error(
        TypingFeedback.SubtypeExpected(
          argument.tpe,
          InferenceVariable.instantiateCandidate(parameterType, assignments),
          argument.position,
        )
      )
      return None
    }

    typeParameters.foldSome(assignments2) { case (assignments3, iv) =>
      Unification.unifyInferenceVariableBounds(iv, assignments3).orElse {
        reporter.error(
          TypingFeedback.IllegalBounds(
            InferenceVariable.instantiateCandidate(iv, assignments3),
            iv.name,
            InferenceVariable.instantiateByBound(iv.lowerBound, BoundType.Lower, assignments3),
            InferenceVariable.instantiateByBound(iv.upperBound, BoundType.Upper, assignments3),
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
    function: FunctionIdentity,
    argumentTypes: Vector[Type],
  )(illegalArity: => Feedback.Error)(implicit reporter: Reporter): Option[TypeVariable.Assignments] = {
    if (argumentTypes.length != function.arity) {
      reporter.error(illegalArity)
      return None
    }

    Fit.fitsAssignments(TupleType(argumentTypes), function.inputType)
  }

}
