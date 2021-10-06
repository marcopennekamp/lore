package lore.compiler.typing

import lore.compiler.feedback.Reporter
import lore.compiler.inference.Inference.Assignments
import lore.compiler.inference.InferenceVariable
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.types.{BasicType, TupleType, Type, TypeVariable}
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
    arguments.foldLeft((Vector.empty: KnownArgumentTypes, assignments)) {
      case ((knownArgumentTypes, previousAssignments), argument) => Synthesizer.attempt(argument, previousAssignments) match {
        case Some(argumentAssignments) => (knownArgumentTypes :+ Some(Helpers.instantiate(argument, argumentAssignments)), argumentAssignments)
        case None => (knownArgumentTypes :+ None, previousAssignments)
      }
    }
  }

  case class ArgumentCandidate(tpe: TupleType, typeVariableAssignments: TypeVariable.Assignments, assignments: Assignments)

  /**
    * Infers the argument type of a function call given `signature`, `arguments`, and the preprocessed
    * `knownArgumentTypes`.
    *
    * The result includes the actual argument type and the type variable assignments, which can be used to perform
    * dispatch, or to assign type arguments in a constructor call.
    */
  def inferArgumentType(
    signature: FunctionSignature,
    arguments: Vector[Expression],
    knownArgumentTypes: KnownArgumentTypes,
    assignments: Assignments,
  )(implicit checker: Checker, reporter: Reporter): Option[ArgumentCandidate] = {
    // TODO (inference): If the function doesn't have type variables, we can simply skip all type variable handling and
    //                   go straight to checking (step 3).

    // (1) Build a working understanding of type variables from the already inferred argument types. We represent type
    //     variables as inference variables, because they are easier to work with.
    val typeVariableAssignments = signature.typeParameters.map(tv => (tv, new InferenceVariable)).toMap
    val parameterTypes = signature.parameters.map(parameter => Type.substitute(parameter.tpe, typeVariableAssignments))

    val (certainArgumentTypes, certainParameterTypes) = knownArgumentTypes.zip(parameterTypes).flatMap {
      case (Some(argumentType), parameterType) => Some((argumentType, Type.substitute(parameterType, typeVariableAssignments)))
      case (None, _) => None
    }.unzip

    val assignments2 = Helpers.unifySubtypes(
      TupleType(certainArgumentTypes),
      TupleType(certainParameterTypes),
      assignments,
    ).getOrElse {
      return None
    }

    // (2) Now that we've assigned the known types to some or all of the function's type parameters, we can narrow
    //     bounds from left to right. For example, if we have a function call `Enum.map([1, 2, 3], x => x + 1)`, we
    //     know that `A = Int`, but the second parameter is typed as `B => C`. To properly `check` the anonymous
    //     function, we have to assign `B >: Int`, which is possible due to the bound `B >: A`. This is why we're
    //     processing bounds here.
    //     We will have to repeat this process each time a new argument has been
    val assignments3 = handleTypeVariableBounds(signature, typeVariableAssignments, assignments2).getOrElse {
      return None
    }

    // (3) With maximum type variable information at hand, we can `check` the untyped arguments from left to right.
    //     In the process, we have to attempt unification for each newly typed argument to further narrow down the type
    //     arguments, and also bounds processing to allow changes in type variable assignments to carry across bounds.
    val assignments4 = knownArgumentTypes.zip(arguments).zip(parameterTypes).foldSome(assignments3) {
      case (innerAssignments, ((None, argument), parameterType)) =>
        checkUntypedArgument(argument, parameterType, innerAssignments)
          .flatMap(handleTypeVariableBounds(signature, typeVariableAssignments, _))

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
    val resultType = Helpers.instantiateCandidate(TupleType(arguments.map(_.tpe)), assignments4).asInstanceOf[TupleType]
    val resultTypeVariableAssignments = typeVariableAssignments.map {
      case (tv, iv) => tv -> Helpers.instantiateCandidate(iv, assignments4)
    }
    val resultAssignments = assignments4.removedAll(typeVariableAssignments.values)
    Some(ArgumentCandidate(resultType, resultTypeVariableAssignments, resultAssignments))
  }

  private def handleTypeVariableBounds(
    signature: FunctionSignature,
    typeVariableAssignments: Map[TypeVariable, InferenceVariable],
    assignments: Assignments,
  ): Option[Assignments] = {
    signature.typeParameters.foldSome(assignments) {
      case (assignments2, tv) => handleTypeVariableBounds(tv, typeVariableAssignments, assignments2)
    }
  }

  private def handleTypeVariableBounds(tv: TypeVariable, typeVariableAssignments: Map[TypeVariable, InferenceVariable], assignments: Assignments): Option[Assignments] = {
    val assignments2 = if (tv.lowerBound != BasicType.Nothing) {
      Helpers.unifySubtypes(Type.substitute(tv.lowerBound, typeVariableAssignments), typeVariableAssignments(tv), assignments)
        .getOrElse(return None)
    } else assignments

    if (tv.upperBound != BasicType.Any) {
      Helpers.unifySubtypes(typeVariableAssignments(tv), Type.substitute(tv.upperBound, typeVariableAssignments), assignments2)
    } else Some(assignments2)
  }

  private def checkUntypedArgument(argument: Expression, parameterType: Type, assignments: Assignments)(implicit checker: Checker, reporter: Reporter) = {
    // TODO (inference): For a call `Enum.map` and its second argument, the candidate instantiation would probably
    //                   result in `Int => Nothing`, because the lower bound is preferred. This has to be `Int => Any`.
    //                   Similarly, for contravariant types, the instantiation also has to instantiate the smart
    //                   default, namely `Nothing`, such as in `Nothing => Int`.
    checker.attempt(argument, Helpers.instantiateCandidate(parameterType, assignments), assignments).flatMap { assignments2 =>
      Helpers.unifySubtypes(Helpers.instantiateCandidate(argument.tpe, assignments2), parameterType, assignments2)
    }
  }

}
