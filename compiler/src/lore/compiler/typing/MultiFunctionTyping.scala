package lore.compiler.typing

import lore.compiler.core.Positioned
import lore.compiler.feedback._
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.typed.Expression
import lore.compiler.semantics.expressions.typed.Expression.{MultiFunctionCall, MultiFunctionValue}
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.semantics.expressions.untyped.UntypedExpression.{UntypedAmbiguousMultiFunctionCall, UntypedMultiFunctionCall}
import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.semantics.modules.MultiReference
import lore.compiler.types.{BasicType, FunctionType, TupleType, Type}
import lore.compiler.typing.CallTyping.{UntypedOrTypedExpression, traceInferredArguments}
import lore.compiler.utils.CollectionExtensions.OptionVectorExtension

object MultiFunctionTyping {

  private case class ArgumentsCandidate(arguments: Vector[Expression], context: InferenceContext) {
    val inputType: TupleType = TupleType(arguments.map(_.tpe))
  }

  def checkOrInferCall(
    expression: UntypedMultiFunctionCall,
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    CallTyping.inferArguments(expression.arguments, context).flatMap { case (arguments, context2) =>
      checkOrInferCall(expression.target, arguments, expectedType, expression, context2)
    }
  }

  /**
    * Checks a multi-function call of `mf`. `expectedType` is used to infer type arguments via the output type in some
    * niche cases.
    *
    * The general approach here is to consider all possible function candidates that inferrable arguments may fit in.
    * Hence, at first inference is attempted for all arguments. If all arguments are immediately typed, we have a
    * trivial multi-function call that can be resolved via dispatch.
    *
    * Otherwise, when an argument cannot be simply inferred, we have to use the type information available from the
    * inferred arguments to type the rest arguments. The method is to pre-filter all possible candidate functions and
    * then resolve the argument types as if this function was chosen. From all fully typed arguments candidates, we
    * finally choose the most specific one. If there is are no most specific arguments candidate, we either have an
    * empty fit or an ambiguity error.
    *
    * If `expectedType` is available, it may inform the assignment of type parameters via unification with the output
    * type of each function candidate. This does not mean that the output type becomes relevant in the multiple
    * dispatch decision: After all argument types have been inferred, multiple dispatch will be simulated to ensure
    * that the call is correct. The test case `test/language/functions/filter-curried.lore` demonstrates how this
    * output-directed inference is useful.
    */
  def checkOrInferCall(
    mf: MultiFunctionDefinition,
    arguments: Vector[UntypedOrTypedExpression],
    expectedType: Option[Type],
    positioned: Positioned,
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    traceCheckOrInferCall(mf, expectedType, positioned) {
      traceInferredArguments(arguments)
      checkOrInferCallImpl(mf, arguments, expectedType, positioned, context)
    }
  }

  def checkOrInferAmbiguousCall(
    expression: UntypedAmbiguousMultiFunctionCall,
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    // Arguments only need to be pre-inferred once, because no type information from the candidate multi-function is
    // required for inference.
    CallTyping.inferArguments(expression.arguments, context).flatMap { case (arguments, context2) =>
      checkOrInferAmbiguousCall(expression.target, arguments, expectedType, expression, context2)
    }
  }

  /**
    * Checks an ambiguous multi-function call of `mfs` using [[MultiReferenceTyping.disambiguate]] and
    * [[checkOrInferCall]].
    */
  def checkOrInferAmbiguousCall(
    mfs: MultiReference[MultiFunctionDefinition],
    arguments: Vector[UntypedOrTypedExpression],
    expectedType: Option[Type],
    positioned: Positioned,
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    Typing.logger.trace(s"Disambiguate multi-function call `${positioned.position.truncatedCode}`" +
      s" (${mfs.local.length} local, ${mfs.global.length} global):")
    Typing.indentationLogger.indented {
      Typing.logger.whenTraceEnabled {
        Typing.logger.trace("Disambiguation candidates:")
        Typing.indentationLogger.indented {
          mfs.local.foreach(mf => Typing.logger.trace(s"- $mf (local)"))
          mfs.global.foreach(mf => Typing.logger.trace(s"- $mf (global)"))
        }
      }

      traceInferredArguments(arguments)
      MultiReferenceTyping.disambiguate(mfs, positioned.position) { case (mf, candidateReporter) =>
        traceCheckOrInferCall(mf, expectedType, positioned) {
          checkOrInferCallImpl(mf, arguments, expectedType, positioned, context)(registry, candidateReporter)
        }
      }
    }
  }

  private def traceCheckOrInferCall[R](
    mf: MultiFunctionDefinition,
    expectedType: Option[Type],
    positioned: Positioned,
  )(f: => R) = {
    Typing.traceCheckOrInfer(s"multi-function call `${mf.name}` in", expectedType, positioned)
    Typing.indentationLogger.indented(f)
  }

  private def checkOrInferCallImpl(
    mf: MultiFunctionDefinition,
    arguments: Vector[UntypedOrTypedExpression],
    expectedType: Option[Type],
    positioned: Positioned,
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    arguments.map(_.toOption).sequence.foreach { arguments =>
      // If all argument types were inferred, we can simply build the call expression.
      Typing.logger.trace("Perform direct dispatch as all arguments have been pre-inferred.")
      return buildMultiFunctionCall(mf, arguments, positioned).map((_, context))
    }

    // TODO: Instead of considering functions from a flat list, walk the dispatch hierarchy...
    val functionCandidates = filterFunctionCandidates(mf, arguments, positioned)
    val (typedArguments, context2) = findArguments(
      mf,
      arguments,
      expectedType,
      functionCandidates,
      positioned,
      context,
    ).getOrElse(return None)

    // (4) Build a call expression from the arguments candidate.
    buildMultiFunctionCall(mf, typedArguments, positioned).map((_, context2))
  }

  /**
    * Filters all function candidates, currently by arity only.
    *
    * TODO: Would it be possible to filter by input type as well? Using `Nothing` for unknown argument types would
    *       usually work, unless a type parameter has a lower bound. This complicates things, but may ultimately be
    *       resolvable. This is also necessary to improve error reports for "sole candidate" calls.
    * TODO (dispatch-consistency): If lower bounds are removed from multi-function type parameters, we can easily
    *                              pre-filter here.
    */
  private def filterFunctionCandidates(
    mf: MultiFunctionDefinition,
    arguments: Vector[UntypedOrTypedExpression],
    positioned: Positioned,
  ): Vector[FunctionDefinition] = {
    mf.functions.filter(_.signature.arity == arguments.length)
  }

  /**
    * Find the actual arguments for a multi-function call by attempting to infer an arguments candidate from each
    * function candidate. If there is only one function candidate, we can improve error reporting by passing through
    * any errors which occur in the arguments. If there are multiple function candidates, we cannot differentiate
    * between a genuine type error inside an argument and a function candidate which does not work with the argument,
    * so there we must default to an empty fit error.
    *
    * TODO: Errors are only passed through when there is a single function candidate. This situation must be improved.
    */
  private def findArguments(
    mf: MultiFunctionDefinition,
    arguments: Vector[UntypedOrTypedExpression],
    expectedType: Option[Type],
    functionCandidates: Vector[FunctionDefinition],
    positioned: Positioned,
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResults] = {
    if (functionCandidates.length == 1) {
      val (argumentsCandidate, feedback) = attemptFunctionCandidate(
        functionCandidates.head,
        arguments,
        expectedType,
        positioned,
        context,
      )
      reporter.report(feedback)
      argumentsCandidate.flatMap(ArgumentsCandidate.unapply)
    } else {
      val argumentsCandidates = functionCandidates.flatMap {
        function => attemptFunctionCandidate(function, arguments, expectedType, positioned, context)._1
      }
      chooseArgumentsCandidate(mf, arguments, argumentsCandidates, positioned).flatMap(ArgumentsCandidate.unapply)
    }
  }

  private def attemptFunctionCandidate(
    function: FunctionDefinition,
    arguments: Vector[UntypedOrTypedExpression],
    expectedType: Option[Type],
    positioned: Positioned,
    context: InferenceContext,
  )(implicit registry: Registry): (Option[ArgumentsCandidate], Vector[Feedback]) = {
    implicit val reporter: MemoReporter = MemoReporter()
    val candidateOption =
      CallTyping.checkOrInfer(function.signature, arguments, expectedType, positioned, context) {
        case (typedArguments, _) => typedArguments
      }
      .map(ArgumentsCandidate.tupled)

    (candidateOption, reporter.feedback)
  }

  private def chooseArgumentsCandidate(
    mf: MultiFunctionDefinition,
    arguments: Vector[UntypedOrTypedExpression],
    argumentsCandidates: Vector[ArgumentsCandidate],
    positioned: Positioned,
  )(implicit reporter: Reporter): Option[ArgumentsCandidate] = {
    if (argumentsCandidates.nonEmpty) {
      val mostSpecific = argumentsCandidates
        .filterNot(candidate => argumentsCandidates.exists(_.inputType < candidate.inputType))
        .distinctBy(_.inputType)

      mostSpecific match {
        case Vector(argumentsCandidate) => Some(argumentsCandidate)
        case _ =>
          Typing.logger.trace(s"Ambiguous argument types of call `${positioned.position.truncatedCode}`:\n" +
            s"${argumentsCandidates.mkString("\n")}")
          reporter.error(
            TypingFeedback.MultiFunctionCall.AmbiguousArgumentTypes(
              mf,
              mostSpecific.map(_.inputType),
              positioned,
            )
          )
          None
      }
    } else {
      Typing.logger.trace(s"Empty fit of call `${positioned.position.truncatedCode}`.")
      val inputType = TupleType(
        arguments.map {
          case Right(typedArgument) => typedArgument.tpe
          case Left(_) => BasicType.Any
        }
      )
      reporter.error(MultiFunctionFeedback.Dispatch.EmptyFit(mf, inputType, positioned))
      None
    }
  }

  /**
    * Performs dispatch on `mf` with the `arguments` and builds a [[MultiFunctionCall]].
    */
  def buildMultiFunctionCall(
    mf: MultiFunctionDefinition,
    arguments: Vector[Expression],
    positioned: Positioned,
  )(implicit reporter: Reporter): Option[Expression] = {
    mf.dispatch(TupleType(arguments.map(_.tpe)), positioned.position)
      .map(instance => MultiFunctionCall(instance, arguments, positioned.position))
  }

  /**
    * Checks or infers a multi-function value of `mf` given `expectedType`.
    */
  def checkOrInferValue(
    mf: MultiFunctionDefinition,
    expression: UntypedExpression,
    expectedType: Option[Type],
  )(implicit reporter: Reporter): Option[MultiFunctionValue] = {
    def build(functionType: FunctionType) = {
      Some(MultiFunctionValue(mf, functionType, expression.position))
    }

    expectedType match {
      case Some(expectedType: FunctionType) =>
        mf.dispatch(expectedType.input, expression.position).flatMap { instance =>
          val functionType = instance.signature.asFunctionType
          if (functionType.output <= expectedType.output) {
            build(functionType)
          } else {
            reporter.error(TypingFeedback.MultiFunctionValue.IllegalOutput(mf, functionType, expectedType, expression))
            None
          }
        }

      case Some(expectedType) if expectedType != BasicType.Any =>
        reporter.error(TypingFeedback.MultiFunctionValue.FunctionTypeExpected(mf, expectedType, expression))
        None

      case _ =>
        // We can infer a multi-function value without a function type context if the multi-function has a single,
        // monomorphic function. This is also a fallback case for an expected type `Any`, which can be a supertype of
        // any function type. So in that case, even if we don't have a function context, the multi-function value might
        // still be typeable.
        mf.functions match {
          case Vector(function) if function.isMonomorphic =>
            build(function.monomorphicInstance.signature.asFunctionType)

          case _ =>
            reporter.error(TypingFeedback.MultiFunctionValue.TypeContextExpected(mf, expression))
            None
        }
    }
  }

  /**
    * Checks or infers a multi-function value of an ambiguous `multiReference` given `expectedType` using
    * [[MultiReferenceTyping.disambiguate]] and [[checkOrInferValue]].
    */
  def checkOrInferAmbiguousValue(
    multiReference: MultiReference[MultiFunctionDefinition],
    expression: UntypedExpression,
    expectedType: Option[Type],
  )(implicit reporter: Reporter): Option[MultiFunctionValue] = {
    MultiReferenceTyping.disambiguate(multiReference, expression.position) { case (mf, candidateReporter) =>
      checkOrInferValue(mf, expression, expectedType)(candidateReporter)
    }
  }

}
