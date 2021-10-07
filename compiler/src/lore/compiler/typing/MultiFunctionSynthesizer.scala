package lore.compiler.typing

import lore.compiler.feedback.{MultiFunctionFeedback, Reporter, TypingFeedback2}
import lore.compiler.inference.Inference.Assignments
import lore.compiler.inference.{Inference, InferenceVariable}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.types.TupleType
import lore.compiler.typing.ParametricFunctionSynthesizer.ArgumentCandidate

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
    val (knownArgumentTypes, assignments2) = ParametricFunctionSynthesizer.preprocessArguments(expression.arguments, assignments)

    // Step 2: Pre-filter all function candidates by arity.
    // TODO (inference): Would it be possible to filter by input type as well? Using `Nothing` for unknown argument
    //                   types would usually work, unless a type parameter has a lower bound. This complicates things,
    //                   but may ultimately be resolvable.
    val functionCandidates = mf.functions.filter(_.signature.arity == expression.arguments.length)

    // Step 3: Handle each function candidate by inferring an argument type from it.
    val argumentCandidates = functionCandidates.flatMap {
      function => ParametricFunctionSynthesizer.inferArgumentType(function.signature, expression.arguments, knownArgumentTypes, assignments2)
    }

    // Step 4: Choose the most specific resulting arguments type and perform dispatch with it, assigning the result
    //         type of the function call on success.
    chooseArgumentCandidate(argumentCandidates, assignments2)
      .flatMap(handleDispatch)
      .getOrElse(assignments2)
  }

  private def chooseArgumentCandidate(argumentCandidates: Vector[ArgumentCandidate], oldAssignments: Assignments): Option[ArgumentCandidate] = {
    if (argumentCandidates.nonEmpty) {
      val mostSpecific = argumentCandidates
        .filterNot(candidate => argumentCandidates.exists(_.tpe < candidate.tpe))
        .distinctBy(_.tpe)

      mostSpecific match {
        case Vector(argumentCandidate) => Some(argumentCandidate)
        case _ =>
          Inference.logger.trace(s"Ambiguous argument types of call at ${expression.position}:\n${argumentCandidates.mkString("\n")}")
          reporter.error(TypingFeedback2.MultiFunctionCalls.AmbiguousArgumentTypes(mf, mostSpecific.map(_.tpe), expression))
          None
      }
    } else {
      Inference.logger.trace(s"Empty fit of call at ${expression.position}:\n${argumentCandidates.mkString("\n")}")
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
