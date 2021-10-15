package lore.compiler.typing.checker

import lore.compiler.feedback.{MultiFunctionFeedback, Reporter, TypingFeedback}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.types.{TupleType, Type}
import lore.compiler.typing.InferenceVariable.Assignments
import lore.compiler.typing.{InferenceVariable, Typing}
import lore.compiler.typing.synthesizer.ParametricFunctionSynthesizer
import lore.compiler.typing.synthesizer.ParametricFunctionSynthesizer.ArgumentCandidate
import lore.compiler.typing.unification.Unification

object MultiFunctionCallChecker {

  /**
    * Checks a multi-function call of `mf` in `expression`, assigning the output type of the multi-function call to
    * `expression`'s type. `expectedType` is used to infer type arguments via the output type in some niche cases.
    */
  def check(
    mf: MultiFunctionDefinition,
    expression: Expression.Call,
    expectedType: Type,
    assignments: Assignments,
  )(implicit checker: Checker, reporter: Reporter): Option[Assignments] = {
    handle(mf, expression, Some(expectedType), assignments)
  }

  /**
    * Checks or infers a multi-function call expression based on whether `expectedType` is available.
    *
    * The general approach here is to consider all possible function candidates that the known arguments may fit in.
    * For that, first inference is attempted for all arguments individually. If all arguments are immediately typed, we
    * have a trivial multi-function call. Otherwise, that is, one argument contains unassigned inference variables, we
    * have to pre-filter all possible candidate functions and then resolve the argument types as if this function was
    * chosen. From all fully typed argument candidates, we finally choose the most specific one. If there is no such
    * candidate, we either have an empty fit or an ambiguity error.
    *
    * If `expectedType` is available, it may inform the assignment of type parameters via unification with the output
    * type of each function candidate. This does not mean that the output type becomes relevant in the multiple
    * dispatch decision: After all argument types have been inferred, multiple dispatch will be simulated to ensure
    * that the call is correct. The test case `test/features/functions/filter-curried.lore` demonstrates how this
    * output-directed inference is useful.
    */
  def handle(
    mf: MultiFunctionDefinition,
    expression: Expression.Call,
    expectedType: Option[Type],
    assignments: Assignments,
  )(implicit checker: Checker, reporter: Reporter): Option[Assignments] = {
    // TODO (inference): Error reporting currently sucks. If we have a call `Enum.map(list, v => ...)` and something is
    //                   wrong inside the anonymous function, the error will simply be swallowed and the user gets a
    //                   generic "empty fit" error. Obviously it's not so easy to pass through the right errors if
    //                   there are multiple function candidates. But if there is only one candidate, we can easily just
    //                   pass through the errors instead of reporting the "empty fit" error. This just requires making
    //                   ParametricFunctionSynthesizer return these errors for failed argument candidates. Inside of
    //                   it, we have to make `checker.attempt` in `checkUntypedArgument` return its errors and then
    //                   return them.
    val modeLabel = expectedType.map(_ => "Checking").getOrElse("Inference")
    val expectedTypeInfo = expectedType.map(t => s" with expected output type `$t`").getOrElse("")
    Typing.logger.trace(s"$modeLabel of multi-function call `${expression.position.truncatedCode}`$expectedTypeInfo:")
    Typing.indentationLogger.indented {
      // Step 1: Try to infer as many argument types as possible.
      // TODO (inference): If all argument types can be inferred trivially, we can simply perform dispatch and call it a
      //                   day. This should significantly improve type checking performance.
      val (knownArgumentTypes, assignments2) = ParametricFunctionSynthesizer.preprocessArguments(expression.arguments, assignments)

      // Step 2: Pre-filter all function candidates by arity.
      // TODO (inference): Would it be possible to filter by input type as well? Using `Nothing` for unknown argument
      //                   types would usually work, unless a type parameter has a lower bound. This complicates things,
      //                   but may ultimately be resolvable.
      val functionCandidates = mf.functions.filter(_.signature.arity == expression.arguments.length)

      // Step 3: Handle each function candidate by inferring an argument type from it.
      val argumentCandidates = functionCandidates.flatMap {
        function =>
          val (typeParameterAssignments, parameterTypes) = ParametricFunctionSynthesizer.prepareParameterTypes(function.signature)

          // As mentioned in the documentation comment, if we have an expected (output) type, we can unify this type
          // with the candidate's output type to potentially assign type arguments right away.
          // TODO (inference): Because sum/intersection type unification cannot be resolved yet, we can't rule out that
          //                   the candidate can't be chosen if the unification fails, here. If that is resolved,
          //                   there's a good chance that we can optimize here: if the output type and expected types
          //                   cannot be unified, the multi-function cannot be chosen.
          val outputTypeAssignments = expectedType match {
            case Some(expectedType) =>
              val outputType = Type.substitute(function.signature.outputType, typeParameterAssignments)
              Unification.unifySubtypes(outputType, expectedType, assignments2)
                .getOrElse(assignments2)

            case None => assignments2
          }

          ParametricFunctionSynthesizer.inferArgumentTypes(
            function.signature.typeParameters,
            typeParameterAssignments,
            parameterTypes,
            expression.arguments,
            knownArgumentTypes,
            outputTypeAssignments,
          )
      }

      // Step 4: Choose the most specific resulting arguments type and perform dispatch with it, assigning the result
      //         type of the function call on success.
      chooseArgumentCandidate(mf, expression, argumentCandidates, assignments2)
        .flatMap(handleDispatch(mf, expression, _))
    }
  }

  private def chooseArgumentCandidate(
    mf: MultiFunctionDefinition,
    expression: Expression.Call,
    argumentCandidates: Vector[ArgumentCandidate],
    oldAssignments: Assignments,
  )(implicit checker: Checker, reporter: Reporter): Option[ArgumentCandidate] = {
    if (argumentCandidates.nonEmpty) {
      val mostSpecific = argumentCandidates
        .filterNot(candidate => argumentCandidates.exists(_.tpe < candidate.tpe))
        .distinctBy(_.tpe)

      mostSpecific match {
        case Vector(argumentCandidate) => Some(argumentCandidate)
        case _ =>
          Typing.logger.trace(s"Ambiguous argument types of call `${expression.position.truncatedCode}`:\n${argumentCandidates.mkString("\n")}")
          reporter.error(TypingFeedback.MultiFunctionCalls.AmbiguousArgumentTypes(mf, mostSpecific.map(_.tpe), expression))
          None
      }
    } else {
      Typing.logger.trace(s"Empty fit of call `${expression.position.truncatedCode}`.")
      val candidate = InferenceVariable.instantiateCandidate(TupleType(expression.arguments.map(_.tpe)), oldAssignments)
      // TODO (inference): Move the error to TypingFeedback?
      reporter.error(MultiFunctionFeedback.Dispatch.EmptyFit(mf, candidate, expression.position))
      None
    }
  }

  private def handleDispatch(
    mf: MultiFunctionDefinition,
    expression: Expression,
    argumentCandidate: ArgumentCandidate,
  )(implicit checker: Checker, reporter: Reporter): Option[Assignments] = {
    mf.dispatch(
      argumentCandidate.tpe,
      // TODO (inference): Move these errors to TypingFeedback?
      MultiFunctionFeedback.Dispatch.EmptyFit(mf, argumentCandidate.tpe, expression.position),
      min => MultiFunctionFeedback.Dispatch.AmbiguousCall(mf, argumentCandidate.tpe, min, expression.position),
    ).flatMap { instance =>
      Typing.logger.trace(s"Assigned output type ${instance.signature.outputType} to result type ${expression.tpe}.")
      InferenceVariable.assign(
        expression.tpe.asInstanceOf[InferenceVariable],
        instance.signature.outputType,
        argumentCandidate.assignments,
      )
    }
  }

}