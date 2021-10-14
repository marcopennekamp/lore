package lore.compiler.typing.synthesizer

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.types.{BasicType, TupleType, Type, TypeVariable}
import lore.compiler.typing.InferenceVariable.Assignments
import lore.compiler.typing.checker.Checker
import lore.compiler.typing.unification.Unification
import lore.compiler.typing.{InferenceVariable, Typing}
import lore.compiler.utils.CollectionExtensions.VectorExtension

object ParametricFunctionSynthesizer {

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
    val typeParameterAssignments = signature.typeParameters.map(tv => (tv, new InferenceVariable)).toMap
    val parameterTypes = signature.parameters.map(parameter => Type.substitute(parameter.tpe, typeParameterAssignments))
    (typeParameterAssignments, parameterTypes)
  }

  case class ArgumentCandidate(tpe: TupleType, typeParameterAssignments: TypeVariable.Assignments, assignments: Assignments)

  /**
    * Infers the argument types of a function call. The `typeParameterAssignments` and `parameterTypes` should be
    * prepared with [[prepareParameterTypes]] and `knownArgumentTypes` should be prepared with [[preprocessArguments]].
    *
    * The result includes the actual argument types as a tuple and the type parameter assignments, which can be used to
    * perform dispatch, or to assign type arguments in a constructor call.
    */
  def inferArgumentTypes(
    typeParameters: Vector[TypeVariable],
    typeParameterAssignments: Map[TypeVariable, InferenceVariable],
    parameterTypes: Vector[Type],
    arguments: Vector[Expression],
    knownArgumentTypes: KnownArgumentTypes,
    assignments: Assignments,
  )(implicit checker: Checker, reporter: Reporter): Option[ArgumentCandidate] = {
    // TODO (inference): If the function doesn't have type variables, we can simply skip all type variable handling and
    //                   go straight to checking (step 3).

    // (1) Build a working understanding of type variables from the already inferred argument types.
    val (certainArgumentTypes, certainParameterTypes) = knownArgumentTypes.zip(parameterTypes).flatMap {
      case (Some(argumentType), parameterType) => Some((argumentType, parameterType))
      case (None, _) => None
    }.unzip

    val assignments2 = Unification.unifyFits(certainArgumentTypes, certainParameterTypes, assignments).getOrElse {
      return None
    }

    // (2) Now that we've assigned the known types to some or all of the function's type parameters, we can narrow
    //     bounds from left to right. For example, if we have a function call `Enum.map([1, 2, 3], x => x + 1)`, we
    //     know that `A = Int`, but the second parameter is typed as `B => C`. To properly `check` the anonymous
    //     function, we have to assign `B >: Int`, which is possible due to the bound `B >: A`. This is why we're
    //     processing bounds here.
    //     We will have to repeat this process each time a new argument has been
    val assignments3 = handleTypeVariableBounds(typeParameters, typeParameterAssignments, assignments2).getOrElse {
      return None
    }

    // (3) With maximum type variable information at hand, we can `check` the untyped arguments from left to right.
    //     In the process, we have to attempt unification for each newly typed argument to further narrow down the type
    //     arguments, and also bounds processing to allow changes in type variable assignments to carry across bounds.
    val assignments4 = knownArgumentTypes.zip(arguments).zip(parameterTypes).foldSome(assignments3) {
      case (innerAssignments, ((None, argument), parameterType)) =>
        Typing.logger.whenTraceEnabled {
          val parameterTypeCandidate = InferenceVariable.instantiateCandidate(parameterType, innerAssignments)
          Typing.logger.trace(s"Check untyped argument `${argument.position.truncatedCode}` with parameter type `$parameterTypeCandidate`:")
        }

        Typing.indentationLogger.indented {
          checkUntypedArgument(argument, parameterType, innerAssignments)
            .flatMap(handleTypeVariableBounds(typeParameters, typeParameterAssignments, _))
        }

      case (innerAssignments, ((Some(_), _), _)) => Some(innerAssignments)
    }.getOrElse {
      return None
    }

    // Note that the resulting type variable assignments are valid only because after checking an untyped argument, we
    // are attempting unification and handling bounds again. For example, for a call `Enum.map` as described in a
    // comment further up, initially, the second argument would be typed as `Int => Any`. This is because there is no
    // assignment to the type parameter `C` yet, meaning it has to be instantiated as the most general type. Only after
    // checking the anonymous function `x => x + 1` do we know that the argument's actual type is `Int => Int` and
    // therefore `C = Int`.
    Some(instantiateResult(arguments.map(_.tpe), typeParameterAssignments, assignments4))
  }

  private def handleTypeVariableBounds(
    typeParameters: Vector[TypeVariable],
    typeParameterAssignments: Map[TypeVariable, InferenceVariable],
    assignments: Assignments,
  ): Option[Assignments] = {
    typeParameters.foldSome(assignments) {
      case (assignments2, tv) => handleTypeVariableBounds(tv, typeParameterAssignments, assignments2)
    }
  }

  private def handleTypeVariableBounds(tv: TypeVariable, typeVariableAssignments: Map[TypeVariable, InferenceVariable], assignments: Assignments): Option[Assignments] = {
    val assignments2 = if (tv.lowerBound != BasicType.Nothing) {
      Unification.unifySubtypes(Type.substitute(tv.lowerBound, typeVariableAssignments), typeVariableAssignments(tv), assignments)
        .getOrElse(return None)
    } else assignments

    if (tv.upperBound != BasicType.Any) {
      Unification.unifySubtypes(typeVariableAssignments(tv), Type.substitute(tv.upperBound, typeVariableAssignments), assignments2)
    } else Some(assignments2)
  }

  private def checkUntypedArgument(argument: Expression, parameterType: Type, assignments: Assignments)(implicit checker: Checker, reporter: Reporter): Option[Assignments] = {
    // TODO (inference): For a call `Enum.map` and its second argument, the candidate instantiation would probably
    //                   result in `Int => Nothing`, because the lower bound is preferred. This has to be `Int => Any`.
    //                   Similarly, for contravariant types, the instantiation also has to instantiate the smart
    //                   default, namely `Nothing`, such as in `Nothing => Int`.
    checker.attempt(argument, InferenceVariable.instantiateCandidate(parameterType, assignments), assignments)._1.flatMap { assignments2 =>
      Unification.unifyFits(InferenceVariable.instantiateCandidate(argument.tpe, assignments2), parameterType, assignments2)
    }
  }

  private def instantiateResult(
    argumentTypes: Vector[Type],
    typeParameterAssignments: Map[TypeVariable, InferenceVariable],
    assignments: Assignments,
  ): ArgumentCandidate = {
    ArgumentCandidate(
      InferenceVariable.instantiateCandidate(TupleType(argumentTypes), assignments).asInstanceOf[TupleType],
      typeParameterAssignments.map {
        case (tv, iv) => tv -> InferenceVariable.instantiateCandidate(iv, assignments)
      },
      assignments.removedAll(typeParameterAssignments.values),
    )
  }

  /**
    * Infers the type arguments of a `signature` given the actual argument types.
    */
  def inferTypeArguments(
    signature: FunctionSignature,
    argumentTypes: Vector[Type],
    assignments: Assignments,
  ): Option[(TypeVariable.Assignments, Assignments)] = {
    val (typeParameterAssignments, parameterTypes) = prepareParameterTypes(signature)
    for {
      assignments2 <- Unification.unifyFits(argumentTypes, parameterTypes, assignments)
      assignments3 <- handleTypeVariableBounds(signature.typeParameters, typeParameterAssignments, assignments2)
      candidate = instantiateResult(argumentTypes, typeParameterAssignments, assignments3)
    } yield (candidate.typeParameterAssignments, candidate.assignments)
  }

}
