package lore.compiler.typing

import lore.compiler.feedback.{MultiFunctionFeedback, Reporter, TypingFeedback2}
import lore.compiler.inference.Inference.Assignments
import lore.compiler.inference.{Inference, InferenceVariable}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.types.{BasicType, TupleType, Type, TypeVariable}
import lore.compiler.utils.CollectionExtensions.VectorExtension

case class MultiFunctionSynthesizer(mf: MultiFunctionDefinition, expression: Expression.Call)(implicit checker: Checker, reporter: Reporter) {

  /**
    * Infers a multi-function call of `mf` in `expression`, assigning the output type of the multi-function call to
    * `expression`'s type.
    *
    * The general approach here is to consider all possible function candidates that the known arguments may fit in.
    * For that, first inference is attempted for all arguments individually. If all arguments are immediately typed, we
    * have a trivial multi-function call. Otherwise, that is, one argument contains unassigned inference variables, we
    * have to pre-filter all possible candidate functions and then resolve the argument types as if this function was
    * chosen. From all fully typed argument candidates, we finally choose the most specific one. If there is no such
    * candidate, we either have an empty fit or an ambiguity error.
    */
  def infer(assignments: Assignments): Assignments = {
    // Step 1: Try to infer as many argument types as possible.
    // TODO (inference): If all argument types can be inferred trivially, we can simply perform dispatch and call it a
    //                   day. This should significantly improve type checking performance.
    var inferredArgumentTypes: Vector[Option[Type]] = Vector.empty
    val assignments2 = expression.arguments.foldLeft(assignments) {
      case (previousAssignments, argument) => Synthesizer.attempt(argument, previousAssignments) match {
        case Some(argumentAssignments) =>
          inferredArgumentTypes = inferredArgumentTypes :+ Some(Helpers.instantiate(argument, argumentAssignments))
          argumentAssignments
        case None =>
          inferredArgumentTypes = inferredArgumentTypes :+ None
          previousAssignments
      }
    }

    // Step 2: Pre-filter all function candidates by arity.
    // TODO (inference): Would it be possible to filter by input type as well? Using `Nothing` for unknown argument
    //                   types would usually work, unless a type parameter has a lower bound. This complicates things,
    //                   but may ultimately be resolvable.
    val functionCandidates = mf.functions.filter(_.signature.arity == expression.arguments.length)

    // Step 3: Handle each function candidate by inferring an argument type from it.
    val argumentCandidates = functionCandidates.flatMap(handleFunctionCandidate(_, inferredArgumentTypes, assignments2))

    // Step 4: Choose the most specific resulting arguments type and perform dispatch with it, assigning the result
    //         type of the function call on success.
    chooseArgumentCandidate(argumentCandidates, assignments2)
      .flatMap(handleDispatch)
      .getOrElse(assignments2)
  }

  private case class ArgumentCandidate(tpe: TupleType, assignments: Assignments)

  // TODO (inference): Perhaps we can use this function to also type struct constructors. We'd just have to take a
  //                   function signature instead of a function definition.
  private def handleFunctionCandidate(
    function: FunctionDefinition,
    inferredArgumentTypes: Vector[Option[Type]],
    assignments: Assignments,
  ): Option[ArgumentCandidate] = {
    // TODO (inference): If the function doesn't have type variables, we can simply skip all type variable handling and
    //                   go straight to checking (step 3).

    // (1) Build a working understanding of type variables from the already inferred argument types. We represent type
    //     variables as inference variables, because they are easier to work with.
    // TODO (inference): Shouldn't we throw away the inference variable assignments for these type variable inference
    //                   variables? Otherwise they'll bloat the assignments map, even though they're only used within
    //                   this current function.
    val typeVariableAssignments = function.typeParameters.map(tv => (tv, new InferenceVariable)).toMap
    val parameterTypes = function.signature.parameters.map(parameter => Type.substitute(parameter.tpe, typeVariableAssignments))

    val (knownArgumentTypes, knownParameterTypes) = inferredArgumentTypes.zip(parameterTypes).flatMap {
      case (Some(argumentType), parameterType) => Some((argumentType, Type.substitute(parameterType, typeVariableAssignments)))
      case (None, _) => None
    }.unzip

    val assignments2 = Helpers.unifyEquals(
      TupleType(knownArgumentTypes),
      TupleType(knownParameterTypes),
      assignments,
    ).getOrElse(return None)

    // (2) Now that we've assigned the known types to some or all of the function's type parameters, we can narrow
    //     bounds from left to right. For example, if we have a function call `Enum.map([1, 2, 3], x => x + 1)`, we
    //     know that `A = Int`, but the second parameter is typed as `B => C`. To properly `check` the anonymous
    //     function, we have to assign `B >: Int`, which is possible due to the bound `B >: A`. This is why we're
    //     processing bounds here.
    // TODO (inference): Don't we have to process these bounds after each successive argument has been typed?
    val assignments3 = handleTypeVariableBounds(function, typeVariableAssignments, assignments2).getOrElse(return None)

    // (3) With maximum type variable information at hand, we can `check` the untyped arguments from left to right.
    val assignments4 = inferredArgumentTypes.zip(expression.arguments).zip(parameterTypes).foldSome(assignments3) {
      case (innerAssignments, ((None, argument), parameterType)) =>
        checker.attempt(argument, Helpers.instantiateCandidate(parameterType, innerAssignments), innerAssignments)

      case (innerAssignments, ((Some(_), _), _)) => Some(innerAssignments)
    }.getOrElse(return None)

    // (4) The resulting assignments can now be used to fully instantiate the argument types. The resulting argument
    //     type ultimately becomes one of the results of this function. In addition, we also remove all type variable
    //     inference variables from the assignments, as they won't be needed anymore.
    val resultType = Helpers.instantiateCandidate(TupleType(expression.arguments.map(_.tpe)), assignments4).asInstanceOf[TupleType]
    val resultAssignments = assignments4.removedAll(typeVariableAssignments.values)
    Some(ArgumentCandidate(resultType, resultAssignments))
  }

  private def handleTypeVariableBounds(
    function: FunctionDefinition,
    typeVariableAssignments: Map[TypeVariable, InferenceVariable],
    assignments: Assignments,
  ): Option[Assignments] = {
    function.typeParameters.foldSome(assignments) {
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

  private def chooseArgumentCandidate(argumentCandidates: Vector[ArgumentCandidate], oldAssignments: Assignments): Option[ArgumentCandidate] = {
    if (argumentCandidates.nonEmpty) {
      val mostSpecific = argumentCandidates
        .filterNot(candidate => argumentCandidates.exists(_.tpe < candidate.tpe))
        .distinctBy(_.tpe)

      mostSpecific match {
        case Vector(argumentCandidate) => Some(argumentCandidate)
        case _ =>
          Inference.logger.trace(s"Ambiguous argument types of $expression:\n${argumentCandidates.mkString("\n")}")
          reporter.error(TypingFeedback2.MultiFunctions.AmbiguousArgumentTypes(mf, mostSpecific.map(_.tpe), expression))
          None
      }
    } else {
      Inference.logger.trace(s"Empty fit of $expression:\n${argumentCandidates.mkString("\n")}")
      val candidate = Inference.instantiateCandidateType(oldAssignments, TupleType(expression.arguments.map(_.tpe)))
      // TODO (inference): Move the error to TypingFeedback?
      reporter.error(MultiFunctionFeedback.Dispatch.EmptyFit(mf, candidate, expression.position))
      None
    }
  }

  private def handleDispatch(argumentCandidate: ArgumentCandidate): Option[Assignments] = {
    mf.dispatch(
      argumentCandidate.tpe,
      // TODO (inference): Move these errors to TypingFeedback?
      MultiFunctionFeedback.Dispatch.EmptyFit(mf, argumentCandidate.tpe, expression.position),
      min => MultiFunctionFeedback.Dispatch.AmbiguousCall(mf, argumentCandidate.tpe, min, expression.position),
    ).flatMap { instance =>
      Inference.logger.trace(s"Assign output type ${instance.signature.outputType} of function instance ${instance.definition} to result type ${expression.tpe}.")
      Helpers.assign(
        expression.tpe.asInstanceOf[InferenceVariable],
        instance.signature.outputType,
        argumentCandidate.assignments,
      )
    }
  }

}
