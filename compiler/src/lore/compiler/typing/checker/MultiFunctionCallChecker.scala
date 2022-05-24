package lore.compiler.typing.checker

import lore.compiler.feedback._
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.types.{TupleType, Type}
import lore.compiler.typing.InferenceVariable.Assignments
import lore.compiler.typing.synthesizer.ArgumentSynthesizer
import lore.compiler.typing.synthesizer.ArgumentSynthesizer.KnownArgumentTypes
import lore.compiler.typing.{InferenceVariable, Typing}
import lore.compiler.utils.CollectionExtensions.OptionVectorExtension

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
    val modeLabel = expectedType.map(_ => "Checking").getOrElse("Inference")
    val expectedTypeInfo = expectedType.map(t => s" with expected output type `$t`").getOrElse("")
    Typing.logger.trace(s"$modeLabel of multi-function call `${expression.position.truncatedCode}`$expectedTypeInfo:")
    Typing.indentationLogger.indented {
      // Step 1: Try to infer as many argument types as possible.
      val (knownArgumentTypes, assignments2) = ArgumentSynthesizer.preprocessArguments(expression.arguments, assignments)

      // If all argument types were inferred, we can simply perform dispatch.
      knownArgumentTypes.sequence.foreach { argumentTypes =>
        Typing.indentationLogger.dedent()
        return handleDispatch(mf, expression, TupleType(argumentTypes), assignments2)
      }

      // Step 2: Pre-filter all function candidates by arity.
      // TODO: Would it be possible to filter by input type as well? Using `Nothing` for unknown argument types would
      //       usually work, unless a type parameter has a lower bound. This complicates things, but may ultimately be
      //       resolvable. This is also necessary to improve error reports for "sole candidate" calls.
      val functionCandidates = mf.functions.filter(_.signature.arity == expression.arguments.length)

      // Step 3: Handle each function candidate by inferring an argument type from it. If there is only one function
      //         candidate, we can improve error reporting by passing through any errors which occur in the arguments.
      //         If there are multiple function candidates, we cannot differentiate between a genuine type error inside
      //         an argument and a function candidate which does not work with the argument, so there we must default
      //         to an empty fit error.
      if (functionCandidates.length == 1) {
        val (argumentCandidate, feedback) = handleFunctionCandidate(functionCandidates.head, expression, knownArgumentTypes, expectedType, assignments2)
        reporter.report(feedback)

        // Step 4: Perform dispatch with the argument type, assigning the result type of the function call on success.
        argumentCandidate.flatMap(candidate => handleDispatch(mf, expression, candidate.tpe, candidate.assignments))
      } else {
        val argumentCandidates = functionCandidates.flatMap {
          function => handleFunctionCandidate(function, expression, knownArgumentTypes, expectedType, assignments2)._1
        }

        // Step 4: Choose the most specific resulting arguments type and perform dispatch with it, assigning the result
        //         type of the function call on success.
        chooseArgumentCandidate(mf, expression, argumentCandidates, assignments2)
          .flatMap(candidate => handleDispatch(mf, expression, candidate.tpe, candidate.assignments))
      }
    }
  }

  /**
    * An argument candidate represents a candidate typing of a multi-function call's arguments.
    *
    * @param tpe The instantiated type of all arguments.
    * @param assignments The assignments as a result of typing the arguments.
    */
  case class ArgumentCandidate(tpe: TupleType, assignments: Assignments)

  private def handleFunctionCandidate(
    function: FunctionDefinition,
    expression: Expression.Call,
    knownArgumentTypes: KnownArgumentTypes,
    expectedType: Option[Type],
    assignments: Assignments,
  )(implicit checker: Checker): (Option[ArgumentCandidate], Vector[Feedback]) = {
    val (typeParameterAssignments, parameterTypes) = ArgumentSynthesizer.prepareParameterTypes(function.signature)
    val outputType = {
      if (expectedType.isDefined) Some(Type.substitute(function.signature.outputType, typeParameterAssignments))
      else None
    }

    implicit val reporter: MemoReporter = MemoReporter()
    val argumentCandidate = ArgumentSynthesizer.inferArgumentTypes(
      function.signature.typeParameters,
      typeParameterAssignments,
      parameterTypes,
      expression.arguments,
      knownArgumentTypes,
      outputType,
      expectedType,
      assignments,
      expression,
    ).map { result =>
      val tpe = InferenceVariable.instantiateCandidate(
        TupleType(expression.arguments.map(_.tpe)),
        result.assignments,
      ).asInstanceOf[TupleType]
      ArgumentCandidate(tpe, result.assignments)
    }

    (argumentCandidate, reporter.feedback)
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
      reporter.error(MultiFunctionFeedback.Dispatch.EmptyFit(mf, candidate, expression.position))
      None
    }
  }

  private def handleDispatch(
    mf: MultiFunctionDefinition,
    expression: Expression,
    argumentType: TupleType,
    assignments: Assignments,
  )(implicit checker: Checker, reporter: Reporter): Option[Assignments] = {
    mf.dispatch(
      argumentType,
      MultiFunctionFeedback.Dispatch.EmptyFit(mf, argumentType, expression.position),
      min => MultiFunctionFeedback.Dispatch.AmbiguousCall(mf, argumentType, min, expression.position),
    ).flatMap { instance =>
      Typing.logger.trace(s"Assigned output type ${instance.signature.outputType} to result type ${expression.tpe}.")
      InferenceVariable.assign(
        expression.tpe.asInstanceOf[InferenceVariable],
        instance.signature.outputType,
        assignments,
      )
    }
  }

}
