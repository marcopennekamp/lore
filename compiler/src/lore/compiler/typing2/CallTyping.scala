package lore.compiler.typing2

import lore.compiler.core.Positioned
import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.semantics.expressions.untyped.UntypedExpression.UntypedCall
import lore.compiler.semantics.functions.FunctionLike
import lore.compiler.types.{TupleType, Type, TypeVariable}
import lore.compiler.typing2.unification.{InferenceAssignments, InferenceVariable2, Unification2}
import lore.compiler.utils.CollectionExtensions.{Tuple2OptionExtension, Tuple3Extension, VectorExtension}

object CallTyping {

  /**
    * TODO (multi-import): Document.
    */
  def checkOrInfer(
    function: FunctionLike,
    expression: UntypedCall,
    expectedType: Option[Type],
    context: InferenceContext,
  )(
    buildCallExpression: (Vector[Expression], TypeVariable.Assignments) => Expression,
  )(implicit checker: Checker2, registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    if (!checkArity(expression.arity, function.arity, expression)) {
      return None
    }

    if (function.isMonomorphic) {
      return inferMonomorphic(function, expression, context)(buildCallExpression)
    }

    val result = withPreparedParameterTypes(function) {
      case (parameterTypes, outputType, typeParameters) =>
        inferArguments(expression, parameterTypes, typeParameters, context).flatMap {
          case (inferredArguments, assignments, context2) =>
            // TODO (multi-import): Move this code to separate helper function?
            // If we have an expected output type, we can unify this type with the function's output type to potentially
            // assign additional type arguments. As mentioned above, the known argument types have to be checked first.
            val assignments2 = expectedType match {
              case Some(expectedType) =>
                Unification2.unifySubtypes(outputType, expectedType, assignments).getOrElse(assignments)
              case _ => assignments
            }

            // Now we can `check` the untyped arguments from left to right. In the process, we have to attempt
            // unification for each newly typed argument to further narrow down the type arguments, and also bounds
            // processing to allow changes in inference variable assignments to carry across bounds. The unification also
            // ensures that each argument type fits the parameter type, which until this point had only been established
            // for inferred arguments.
            val (typedArguments, assignments3, context3) = inferredArguments
              .zip(expression.arguments)
              .zip(parameterTypes)
              .foldLeft((Vector.empty[Expression], assignments2, context2)) {
                case ((typedArguments, assignments3, context3), ((None, argument), parameterType)) =>
                  checkArgument(argument, parameterType, typeParameters, assignments3, context3)
                    .getOrElse {
                      reporter.error(TypingFeedback.Call.IllegalArgumentType(None, parameterType, argument))
                      return None
                    }
                    .mapFirst(typedArguments :+ _)

                case (result, ((Some(typedArgument), _), _)) => result.mapFirst(_ :+ typedArgument)
              }

            Some(assignments3, (typedArguments, context3))
        }
    }

    result.map { case (typeVariableAssignments, (typedArguments, context2)) =>
      (buildCallExpression(typedArguments, typeVariableAssignments), context2)
    }
  }

  private def checkArity(
    argumentCount: Int,
    parameterCount: Int,
    positioned: Positioned,
  )(implicit reporter: Reporter): Boolean = {
    if (argumentCount != parameterCount) {
      reporter.error(TypingFeedback.Call.IllegalArity(argumentCount, parameterCount, positioned))
      false
    } else true
  }

  /**
    * An optimized version of [[checkOrInfer]] in case the signature is monomorphic. Inferring the call of a function
    * that doesn't contain any type parameters is much easier and thereby faster. [[inferMonomorphic]] doesn't check
    * for arity as [[checkOrInfer]] must already have handled this.
    */
  private def inferMonomorphic(
    function: FunctionLike,
    expression: UntypedExpression.UntypedCall,
    context: InferenceContext,
  )(
    buildCallExpression: (Vector[Expression], TypeVariable.Assignments) => Expression,
  )(implicit checker: Checker2, reporter: Reporter): Option[InferenceResult] = {
    if (expression.arity != function.arity) {
      reporter.error(TypingFeedback.Function.IllegalArity(expression.arity, function.arity, expression))
      return None
    }

    expression.arguments.zip(function.parameterTypes)
      .foldSomeCollect(context) {
        case (context, (argument, parameterType)) => checker.check(argument, parameterType, context)
      }
      .mapFirst(typedArguments => buildCallExpression(typedArguments, Map.empty))
  }

  /**
    * Prepares `signature`'s parameter types for call typing by replacing type variables with inference variables. Also
    * returns the prepared output type, and the inference variables representing each type parameter in order.
    *
    * TODO (multi-import): Does this even need to be a separate function? Is this used anywhere else?
    */
  private def withPreparedParameterTypes[R](function: FunctionLike)(
    f: (Vector[Type], Type, Vector[InferenceVariable2]) => Option[(InferenceAssignments, R)],
  ): Option[(TypeVariable.Assignments, R)] = {
    val (parameterTypeTuple, tvToIv) = InferenceVariable2.fromTypeVariables(
      function.inputType,
      function.typeParameters,
    )
    val parameterTypes = parameterTypeTuple.asInstanceOf[TupleType].elements
    val outputType = Type.substitute(function.outputType, tvToIv)
    val inferenceVariables = function.typeParameters.map(tvToIv)

    f(parameterTypes, outputType, inferenceVariables).map { case (inferenceAssignments, result) =>
      val typeVariableAssignments = tvToIv.map {
        case (tv, iv) => tv -> InferenceVariable2.instantiateCandidate(iv, inferenceAssignments)
      }
      (typeVariableAssignments, result)
    }
  }

  /**
    * Attempts to infer `expression.arguments` without a type context. The resulting list contains an inferred
    * expression for each argument, or `None` if the argument couldn't be inferred. The resulting inference assignments
    * contain (partial) bounds for `typeParameters`.
    *
    * @param parameterTypes These parameter types must have been prepared using [[withPreparedParameterTypes]].
    *
    * TODO (multi-import): Can this be private?
    */
  private def inferArguments(
    expression: UntypedCall,
    parameterTypes: Vector[Type],
    typeParameters: Vector[InferenceVariable2],
    context: InferenceContext,
  )(
    implicit checker: Checker2,
    registry: Registry,
    reporter: Reporter,
  ): Option[(Vector[Option[Expression]], InferenceAssignments, InferenceContext)] = {
    val (inferredArguments, context2) = expression.arguments.foldLeft((Vector.empty[Option[Expression]], context)) {
      case ((typedArguments, context2), argument) =>
        Synthesizer2.attempt(argument, context2)._1 match {
          case Some((typedArgument, context3)) => (typedArguments :+ Some(typedArgument), context3)
          case None => (typedArguments :+ None, context2)
        }
    }

    val (inferredArgumentTypes, inferrableParameterTypes) = inferredArguments
      .zip(parameterTypes)
      .flatMap {
        case (Some(expression), parameterType) => Some(expression.tpe, parameterType)
        case _ => None
      }
      .unzip

    unifyArgumentTypes(
      inferredArgumentTypes,
      inferrableParameterTypes,
      typeParameters,
      Map.empty,
      expression,
    ).map { assignments =>
      (inferredArguments, assignments, context2)
    }
  }

  /**
    * Unifies `argumentTypes` with `parameterTypes`, checking their fit and the bounds of all `typeParameters`.
    *
    * @param parameterTypes These parameter types must have been prepared using [[withPreparedParameterTypes]].
    *
    * TODO (multi-import): Turn `argumentTypes` into `arguments: Vector[Expression]`?
    * TODO (multi-import): Can this be private?
    */
  private def unifyArgumentTypes(
    argumentTypes: Vector[Type],
    parameterTypes: Vector[Type],
    typeParameters: Vector[InferenceVariable2],
    assignments: InferenceAssignments,
    positioned: Positioned,
  )(implicit reporter: Reporter): Option[InferenceAssignments] = {
    // This unification has two purposes:
    //   1. It checks that each argument type fits the parameter type.
    //   2. It assigns type arguments to type parameters.
    val assignments2 = Unification2.unifyFits(argumentTypes, parameterTypes, assignments).getOrElse {
      // TODO (multi-import): Process arguments one-by-one so that we can report IllegalArgumentType errors for each
      //                      argument separately.
      // TODO (multi-import): Improve the error by giving a full list of arguments and parameters, NOT a partial list,
      //                      which is the case when `unifyArgumentTypes` is called with a subset of the inferred
      //                      arguments. Reporting this partial list is untenable as it will confuse the user. The
      //                      existence of such a partial list is an implementation detail.
      reporter.error(
        TypingFeedback.Function.IllegalArgumentTypes(
          argumentTypes,
          InferenceVariable2.instantiateCandidate(parameterTypes, assignments),
          positioned,
        )
      )
      return None
    }

    if (typeParameters.nonEmpty) {
      Unification2.unifyInferenceVariableBounds(typeParameters, assignments2).orElse {
        val typeArguments = InferenceVariable2.instantiateCandidate(typeParameters, assignments2)
        reporter.error(TypingFeedback.Function.IllegalTypeArguments(typeArguments, typeParameters, positioned))
        None
      }
    } else Some(assignments2)
  }

  private def checkArgument(
    argument: UntypedExpression,
    parameterType: Type,
    typeParameters: Vector[InferenceVariable2],
    assignments: InferenceAssignments,
    context: InferenceContext,
  )(
    implicit checker: Checker2,
    reporter: Reporter,
  ): Option[(Expression, InferenceAssignments, InferenceContext)] = {
    val parameterTypeCandidate = InferenceVariable2.instantiateCandidate(parameterType, assignments)
    Typing2.logger.trace(s"Check untyped argument `${argument.position.truncatedCode}` with parameter type" +
      s" `$parameterTypeCandidate`:")

    Typing2.indentationLogger.indented {
      checker.check(argument, parameterTypeCandidate, context).flatMap { case (typedArgument, context2) =>
        // Unification only makes sense when the function has type parameters, as the parameter type won't contain any
        // inference variables if it doesn't.
        if (typeParameters.nonEmpty) {
          Unification2.unifyFits(typedArgument.tpe, parameterType, assignments)
            .flatMap(Unification2.unifyInferenceVariableBounds(typeParameters, _))
            .map(assignments2 => (typedArgument, assignments2, context2))
        } else Some(typedArgument, assignments, context2)
      }
    }
  }

  /**
    * Infers the type arguments of a `signature` given the actual argument types. If the argument types don't fit the
    * parameter types, `inferTypeArguments` reports an appropriate error.
    *
    * TODO (multi-import): Where is this actually used? Also, CallTyping might not be the right place for this function.
    */
  def inferTypeArguments(
    function: FunctionLike,
    argumentTypes: Vector[Type],
    context: InferenceContext,
  )(implicit checker: Checker2, reporter: Reporter): Option[Vector[Type]] = {
    // TODO (multi-import): Just do `withPreparedParameterTypes { ... => unifyArgumentTypes }` (and also check for
    //                      arity, though with a different error message than in checkArity). That should work, as it's
    //                      essentially the same implementation as the old one (prepareParameterTypes and then
    //                      checkArgumentTypes).
    ???
  }

}
