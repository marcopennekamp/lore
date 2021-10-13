package lore.compiler.typing.synthesizer

import lore.compiler.feedback.{MultiFunctionFeedback, Reporter, TypingFeedback2}
import lore.compiler.inference.Inference.Assignments
import lore.compiler.inference.{Inference, InferenceVariable}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.types.TupleType
import lore.compiler.typing.InferenceVariable2
import lore.compiler.typing.checker.Checker
import lore.compiler.typing.synthesizer.ParametricFunctionSynthesizer.ArgumentCandidate

case class MultiFunctionCallSynthesizer(mf: MultiFunctionDefinition, expression: Expression.Call)(implicit checker: Checker, reporter: Reporter) {

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
  def infer(assignments: Assignments): Option[Assignments] = {
    // TODO (inference): Error reporting currently sucks. If we have a call `Enum.map(list, v => ...)` and something is
    //                   wrong inside the anonymous function, the error will simply be swallowed and the user gets a
    //                   generic "empty fit" error. Obviously it's not so easy to pass through the right errors if
    //                   there are multiple function candidates. But if there is only one candidate, we can easily just
    //                   pass through the errors instead of reporting the "empty fit" error. This just requires making
    //                   ParametricFunctionSynthesizer return these errors for failed argument candidates. Inside of
    //                   it, we have to make `checker.attempt` in `checkUntypedArgument` return its errors and then
    //                   return them.
    Inference.logger.trace(s"Inference of multi-function call `${expression.position.truncatedCode}`:")
    Inference.indentationLogger.indented {
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
          ParametricFunctionSynthesizer.inferArgumentTypes(function.signature.typeParameters, typeParameterAssignments, parameterTypes, expression.arguments, knownArgumentTypes, assignments2)
      }

      // Step 4: Choose the most specific resulting arguments type and perform dispatch with it, assigning the result
      //         type of the function call on success.
      chooseArgumentCandidate(argumentCandidates, assignments2)
        .flatMap(handleDispatch)
    }
  }

  private def chooseArgumentCandidate(argumentCandidates: Vector[ArgumentCandidate], oldAssignments: Assignments): Option[ArgumentCandidate] = {
    if (argumentCandidates.nonEmpty) {
      val mostSpecific = argumentCandidates
        .filterNot(candidate => argumentCandidates.exists(_.tpe < candidate.tpe))
        .distinctBy(_.tpe)

      mostSpecific match {
        case Vector(argumentCandidate) => Some(argumentCandidate)
        case _ =>
          Inference.logger.trace(s"Ambiguous argument types of call `${expression.position.truncatedCode}`:\n${argumentCandidates.mkString("\n")}")
          reporter.error(TypingFeedback2.MultiFunctionCalls.AmbiguousArgumentTypes(mf, mostSpecific.map(_.tpe), expression))
          None
      }
    } else {
      Inference.logger.trace(s"Empty fit of call `${expression.position.truncatedCode}`.")
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
      Inference.logger.trace(s"Assigned output type ${instance.signature.outputType} to result type ${expression.tpe}.")
      InferenceVariable2.assign(
        expression.tpe.asInstanceOf[InferenceVariable],
        instance.signature.outputType,
        argumentCandidate.assignments,
      )
    }
  }

}
