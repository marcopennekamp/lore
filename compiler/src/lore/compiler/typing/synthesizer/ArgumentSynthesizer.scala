package lore.compiler.typing.synthesizer

/*

import lore.compiler.core.Positioned
import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.semantics.expressions.typed.Expression
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.types.{TupleType, Type, TypeVariable}
import lore.compiler.typing.InferenceVariable.Assignments
import lore.compiler.typing.checker.Checker
import lore.compiler.typing.unification.Unification
import lore.compiler.typing.{InferenceVariable, Typing}
import lore.compiler.utils.CollectionExtensions.VectorExtension

object ArgumentSynthesizer {

  type KnownArgumentTypes = Vector[Option[Type]]

  /**
    * Attempts to infer the types of the given `arguments`. If an argument type cannot be inferred, the resulting
    * "known argument types" list contains `None`.
    */
  def preprocessArguments(
    arguments: Vector[Expression],
    assignments: Assignments,
  )(implicit checker: Checker, reporter: Reporter): (KnownArgumentTypes, Assignments) = {
    val (knownArgumentTypes, assignments2) = arguments.foldLeft((Vector.empty: KnownArgumentTypes, assignments)) {
      case ((knownArgumentTypes, previousAssignments), argument) =>
        Typing.logger.trace(s"Preprocess argument `${argument.position.truncatedCode}`:")

        Typing.indentationLogger.indented {
          Synthesizer.attempt(argument, previousAssignments) match {
            case (Some(argumentAssignments), _) => (knownArgumentTypes :+ Some(InferenceVariable.instantiateCandidate(argument, argumentAssignments)), argumentAssignments)
            case (None, _) => (knownArgumentTypes :+ None, previousAssignments)
          }
        }
    }

    Typing.logger.trace(s"Preprocessed argument types: ${knownArgumentTypes.mkString(", ")}.")

    (knownArgumentTypes, assignments2)
  }

  /**
    * Replaces any type variables in `signature` with inference variables, preparing the signature for inference.
    */
  def prepareParameterTypes(signature: FunctionSignature): (Map[TypeVariable, InferenceVariable], Vector[Type]) = {
    val (parameterType, typeParameterAssignments) = InferenceVariable.fromTypeVariables(
      TupleType(signature.parameters.map(_.tpe)),
      signature.typeParameters,
    )
    (typeParameterAssignments,parameterType.asInstanceOf[TupleType].elements)
  }

  /**
    * @param typeArguments This map contains the instantiated type arguments for all type parameters of the
    *                                 function.
    */
  case class Result(assignments: Assignments, typeArguments: TypeVariable.Assignments)

  private def instantiateResult(
    assignments: Assignments,
    typeParameterAssignments: Map[TypeVariable, InferenceVariable],
  ): Result = {
    Result(
      assignments.removedAll(typeParameterAssignments.values),
      typeParameterAssignments.map {
        case (tv, iv) => tv -> InferenceVariable.instantiateCandidate(iv, assignments)
      },
    )
  }

  /**
    * Checks the argument types and infers type arguments of a function call given an unchecked list of argument types.
    * `typeParameterAssignments` and `parameterTypes` should be prepared with [[prepareParameterTypes]].
    *
    * If the argument count doesn't match the arity of the function, or if the argument types don't fit into the
    * parameter types, `checkArgumentTypes` reports appropriate errors and returns None.
    *
    * Inference variables contained in `typeParameterAssignments` are cleaned from the [[Result]] assignments.
    */
  def checkArgumentTypes(
    typeParameters: Vector[TypeVariable],
    typeParameterAssignments: Map[TypeVariable, InferenceVariable],
    parameterTypes: Vector[Type],
    argumentTypes: Vector[Type],
    assignments: Assignments,
    context: Positioned,
  )(implicit checker: Checker, reporter: Reporter): Option[Result] = {
    checkArgumentTypesImpl(typeParameters, typeParameterAssignments, parameterTypes, argumentTypes, assignments, context)
      .map(instantiateResult(_, typeParameterAssignments))
  }

  /**
    * This implementation of `checkArgumentTypes` doesn't yet instantiate a [[Result]] so that [[inferArgumentTypes]]
    * still has the inference variables of type parameter assignments in the resulting Assignments. This is because
    * [[instantiateResult]] removes inference variables in `typeParameterAssignments` from the resulting assignments.
    */
  private def checkArgumentTypesImpl(
    typeParameters: Vector[TypeVariable],
    typeParameterAssignments: Map[TypeVariable, InferenceVariable],
    parameterTypes: Vector[Type],
    argumentTypes: Vector[Type],
    assignments: Assignments,
    context: Positioned,
  )(implicit checker: Checker, reporter: Reporter): Option[Assignments] = {
    if (argumentTypes.length != parameterTypes.length) {
      reporter.error(TypingFeedback.Function.IllegalArity(argumentTypes.length, parameterTypes.length, context))
      return None
    }

    // This unification has two purposes:
    //   1. It checks that each argument type fits the parameter type.
    //   2. It assigns type arguments to type parameters.
    val assignments2 = Unification.unifyFits(argumentTypes, parameterTypes, assignments).getOrElse {
      reporter.error(
        TypingFeedback.Function.IllegalArgumentTypes(
          InferenceVariable.instantiateCandidate(argumentTypes, assignments),
          InferenceVariable.instantiateCandidate(parameterTypes, assignments),
          context,
        )
      )
      return None
    }

    val assignments3 = if (typeParameters.nonEmpty) {
      Unification.unifyTypeVariableBounds(typeParameters, typeParameterAssignments, assignments2).getOrElse {
        val typeArguments = InferenceVariable.instantiateCandidate(typeParameters.map(typeParameterAssignments), assignments2)
        reporter.error(TypingFeedback.Function.IllegalTypeArguments(typeArguments, typeParameters, context))
        return None
      }
    } else assignments2

    Some(assignments3)
  }

  /**
    * Infers the argument types of a function call. The `typeParameterAssignments` and `parameterTypes` should be
    * prepared with [[prepareParameterTypes]] and `knownArgumentTypes` should be prepared with [[preprocessArguments]].
    *
    * If the argument count doesn't match the arity of the function, or if the inferred argument types don't fit into
    * the parameter types, `inferArgumentTypes` reports appropriate errors and returns None.
    *
    * If an expected output type is given, it informs the function call's type parameter assignments <i>after</i> known
    * argument types have been processed. This is important for error reporting when a function call's output type and
    * the expected output type don't match, even if the function call itself is well typed. Processing the expected
    * output type first creates a confusing error that assumes the known argument type to be incorrect, instead of the
    * output type itself being reported as incorrect. For example, in one case, an error that a `max_by!` call didn't
    * return the expected type `A` but rather `(A, B)` was reported as "`[(Foo, Bar)]` does not fit into `[Foo]`",
    * because the expected type was applied first. The correct error would be "Expected `Foo`, but got `(Foo, Bar)`",
    * which is the error reported after fixing this issue.
    *
    * Inference variables contained in `typeParameterAssignments` are cleaned from the [[Result]] assignments.
    */
  def inferArgumentTypes(
    typeParameters: Vector[TypeVariable],
    typeParameterAssignments: Map[TypeVariable, InferenceVariable],
    parameterTypes: Vector[Type],
    arguments: Vector[Expression],
    knownArgumentTypes: KnownArgumentTypes,
    outputType: Option[Type],
    expectedOutputType: Option[Type],
    assignments: Assignments,
    context: Positioned,
  )(implicit checker: Checker, reporter: Reporter): Option[Result] = {
    // (1) First we build a working understanding of the function call's type arguments from already known argument
    //     types. `checkArgumentTypes` allows us to check the fit of the certain argument types and also assign all
    //     possible type arguments in one go.
    //
    //     In the process, type bounds are narrowed from left to right. For example, if we have a function call
    //     `Enum.map([1, 2, 3], x => x + 1)`, we know that `A = Int`, but the second parameter is typed as `B => C`. To
    //     properly `check` the anonymous function, we have to assign `B >: Int`, which is possible due to the bound
    //     `B >: A`. This is why we're processing bounds of known argument types first.
    //
    //     For unknown argument types (i.e. those with None entries in `knownArgumentTypes`), the fits and bounds
    //     checks are later carried out by `handleUntypedArgument`.
    val (certainArgumentTypes, certainParameterTypes) = knownArgumentTypes.zip(parameterTypes).flatMap {
      case (Some(argumentType), parameterType) => Some((argumentType, parameterType))
      case (None, _) => None
    }.unzip

    val assignments2 = checkArgumentTypesImpl(
      typeParameters, typeParameterAssignments, certainParameterTypes, certainArgumentTypes, assignments, context
    ).getOrElse {
      // `checkArgumentTypes` will have already reported an error.
      return None
    }

    // If we have an expected output type, we can unify this type with the function's output type to potentially assign
    // additional type arguments. As mentioned above, the known argument types have to be checked first.
    val assignments3 = (outputType, expectedOutputType) match {
      case (Some(outputType), Some(expectedOutputType)) =>
        Unification.unifySubtypes(outputType, expectedOutputType, assignments2).getOrElse(assignments2)
      case _ => assignments2
    }

    // (2) With maximum type variable information at hand, we can `check` the untyped arguments from left to right.
    //     In the process, we have to attempt unification for each newly typed argument to further narrow down the type
    //     arguments, and also bounds processing to allow changes in type variable assignments to carry across bounds.
    //     The unification also ensures that the argument type fits the parameter type, which until this point had only
    //     been established for known argument types.
    val assignments4 = knownArgumentTypes.zip(arguments).zip(parameterTypes).foldSome(assignments3) {
      case (innerAssignments, ((None, argument), parameterType)) =>
        handleUntypedArgument(argument, parameterType, typeParameters, typeParameterAssignments, innerAssignments)
      case (innerAssignments, ((Some(_), _), _)) => Some(innerAssignments)
    }.getOrElse {
      reporter.error(
        TypingFeedback.Function.IllegalArgumentTypes(
          InferenceVariable.instantiateCandidate(arguments.map(_.tpe), assignments3),
          InferenceVariable.instantiateCandidate(parameterTypes, assignments3),
          context,
        )
      )
      return None
    }

    // Note that the resulting type parameter assignments are valid only because after checking an untyped argument, we
    // are attempting unification and handling bounds again. For example, for a call `Enum.map` described in the
    // comment for step (1), initially the second argument would be typed as `Number => Any`. This is because there is
    // no assignment to the type parameter `C` yet, meaning it has to be instantiated as the most general type. Only
    // after checking the anonymous function `x => x + 1` do we know that the argument's actual type is
    // `Number => Number` and therefore `C = Number`.
    Some(instantiateResult(assignments4, typeParameterAssignments))
  }

  private def handleUntypedArgument(
    argument: Expression,
    parameterType: Type,
    typeParameters: Vector[TypeVariable],
    typeParameterAssignments: Map[TypeVariable, InferenceVariable],
    assignments: Assignments,
  )(implicit checker: Checker, reporter: Reporter): Option[Assignments] = {
    Typing.logger.whenTraceEnabled {
      val parameterTypeCandidate = InferenceVariable.instantiateCandidate(parameterType, assignments)
      Typing.logger.trace(s"Check untyped argument `${argument.position.truncatedCode}` with parameter type `$parameterTypeCandidate`:")
    }

    Typing.indentationLogger.indented {
      checker.check(argument, InferenceVariable.instantiateCandidate(parameterType, assignments), assignments).flatMap { assignments2 =>
        // `unifyFits` only makes sense when the function has type parameters, as the parameter type won't contain any
        // inference variables if it doesn't.
        if (typeParameters.nonEmpty) {
          Unification.unifyFits(InferenceVariable.instantiateCandidate(argument.tpe, assignments2), parameterType, assignments2)
            .flatMap(Unification.unifyTypeVariableBounds(typeParameters, typeParameterAssignments, _))
        } else Some(assignments2)
      }
    }
  }

  /**
    * Infers the type arguments of a `signature` given the actual argument types.
    *
    * If the argument types don't fit the parameter types, `inferTypeArguments` reports an appropriate error.
    */
  def inferTypeArguments(
    signature: FunctionSignature,
    argumentTypes: Vector[Type],
    assignments: Assignments,
    context: Positioned,
  )(implicit checker: Checker, reporter: Reporter): Option[Result] = {
    val (typeParameterAssignments, parameterTypes) = prepareParameterTypes(signature)
    checkArgumentTypes(
      signature.typeParameters,
      typeParameterAssignments,
      parameterTypes,
      argumentTypes,
      assignments,
      context,
    )
  }

}

*/
