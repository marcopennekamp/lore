package lore.compiler.typing

import lore.compiler.core.Position
import lore.compiler.feedback._
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.typed.Expression.{MultiFunctionCall, MultiFunctionValue}
import lore.compiler.semantics.expressions.typed.Expression
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.semantics.expressions.untyped.UntypedExpression.{UntypedAmbiguousMultiFunctionCall, UntypedCall, UntypedMultiFunctionCall}
import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.semantics.modules.MultiReference
import lore.compiler.types.{BasicType, FunctionType, TupleType, Type}
import lore.compiler.utils.CollectionExtensions.OptionVectorExtension

object MultiFunctionTyping {

  private case class ArgumentsCandidate(arguments: Vector[Expression], context: InferenceContext) {
    val inputType: TupleType = TupleType(arguments.map(_.tpe))
  }

  /**
    * Checks a multi-function call `expression`. `expectedType` is used to infer type arguments via the output type in
    * some niche cases.
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
    expression: UntypedMultiFunctionCall,
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    checkOrInferCall(expression.target, expression, expectedType, context)
  }

  /**
    * Checks an ambiguous multi-function call `expression` using [[MultiReferenceTyping.disambiguate]] and
    * [[checkOrInferCall]].
    */
  def checkOrInferAmbiguousCall(
    expression: UntypedAmbiguousMultiFunctionCall,
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    val mfs = expression.target
    Typing.logger.trace(s"Disambiguate multi-function call `${expression.position.truncatedCode}`" +
      s" (${mfs.local.length} local, ${mfs.global.length} global):")
    Typing.indentationLogger.indented {
      Typing.logger.whenTraceEnabled {
        Typing.logger.trace("Disambiguation candidates:")
        Typing.indentationLogger.indented {
          mfs.local.foreach(mf => Typing.logger.trace(s"- $mf (local)"))
          mfs.global.foreach(mf => Typing.logger.trace(s"- $mf (global)"))
        }
      }

      // TODO (multi-import): Only pre-infer argument types once instead of processing them with each individual
      //                      multi-function.
      MultiReferenceTyping.disambiguate(mfs, expression.position) { case (mf, candidateReporter) =>
        checkOrInferCall(mf, expression, expectedType, context)(registry, candidateReporter)
      }
    }
  }

  private def checkOrInferCall(
    mf: MultiFunctionDefinition,
    expression: UntypedCall,
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    Typing.traceCheckOrInfer(s"multi-function call `${mf.name}` in", expression, expectedType)
    Typing.indentationLogger.indented {
      // Being a separate *Impl function allows it to return early without disturbing the automatic dedent of the
      // indentation logger.
      checkOrInferCallImpl(mf, expression, expectedType, context)
    }
  }

  private def checkOrInferCallImpl(
    mf: MultiFunctionDefinition,
    expression: UntypedCall,
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    val (inferredArguments, context2) = CallTyping.inferArguments(expression, context).getOrElse(return None)
    inferredArguments.sequence.foreach { arguments =>
      // If all argument types were inferred, we can simply build the call expression.
      Typing.logger.trace("Perform direct dispatch as all arguments have been pre-inferred.")
      return buildMultiFunctionCall(mf, arguments, expression.position).map((_, context2))
    }

    // TODO: Instead of considering functions from a flat list, walk the dispatch hierarchy...
    val functionCandidates = filterFunctionCandidates(mf, expression, inferredArguments)
    val (arguments, context3) = findArguments(
      mf,
      expression,
      expectedType,
      inferredArguments,
      functionCandidates,
      context2,
    ).getOrElse(return None)

    // (4) Build a call expression from the arguments candidate.
    buildMultiFunctionCall(mf, arguments, expression.position).map((_, context3))
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
    expression: UntypedCall,
    inferredArguments: Vector[Option[Expression]],
  ): Vector[FunctionDefinition] = {
    mf.functions.filter(_.signature.arity == expression.arity)
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
    expression: UntypedCall,
    expectedType: Option[Type],
    inferredArguments: Vector[Option[Expression]],
    functionCandidates: Vector[FunctionDefinition],
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResults] = {
    if (functionCandidates.length == 1) {
      val (argumentsCandidate, feedback) = attemptFunctionCandidate(
        functionCandidates.head,
        expression,
        inferredArguments,
        expectedType,
        context,
      )
      reporter.report(feedback)
      argumentsCandidate.flatMap(ArgumentsCandidate.unapply)
    } else {
      val argumentsCandidates = functionCandidates.flatMap {
        function => attemptFunctionCandidate(function, expression, inferredArguments, expectedType, context)._1
      }
      chooseArgumentsCandidate(mf, expression, inferredArguments, argumentsCandidates).flatMap(ArgumentsCandidate.unapply)
    }
  }

  private def attemptFunctionCandidate(
    function: FunctionDefinition,
    expression: UntypedCall,
    inferredArguments: Vector[Option[Expression]],
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit registry: Registry): (Option[ArgumentsCandidate], Vector[Feedback]) = {
    implicit val reporter: MemoReporter = MemoReporter()
    val candidateOption = CallTyping
      .checkOrInfer(function.signature, expression, inferredArguments, expectedType, context) {
        case (typedArguments, _) => typedArguments
      }
      .map(ArgumentsCandidate.tupled)

    (candidateOption, reporter.feedback)
  }

  private def chooseArgumentsCandidate(
    mf: MultiFunctionDefinition,
    expression: UntypedCall,
    inferredArguments: Vector[Option[Expression]],
    argumentsCandidates: Vector[ArgumentsCandidate],
  )(implicit reporter: Reporter): Option[ArgumentsCandidate] = {
    if (argumentsCandidates.nonEmpty) {
      val mostSpecific = argumentsCandidates
        .filterNot(candidate => argumentsCandidates.exists(_.inputType < candidate.inputType))
        .distinctBy(_.inputType)

      mostSpecific match {
        case Vector(argumentsCandidate) => Some(argumentsCandidate)
        case _ =>
          Typing.logger.trace(s"Ambiguous argument types of call `${expression.position.truncatedCode}`:\n" +
            s"${argumentsCandidates.mkString("\n")}")
          reporter.error(
            TypingFeedback.MultiFunctionCall.AmbiguousArgumentTypes(
              mf,
              mostSpecific.map(_.inputType),
              expression,
            )
          )
          None
      }
    } else {
      Typing.logger.trace(s"Empty fit of call `${expression.position.truncatedCode}`.")
      val inputType = TupleType(
        inferredArguments.map {
          case Some(typedArgument) => typedArgument.tpe
          case None => BasicType.Any
        }
      )
      reporter.error(MultiFunctionFeedback.Dispatch.EmptyFit(mf, inputType, expression.position))
      None
    }
  }

  /**
    * Performs dispatch on `mf` with the `arguments` and builds a [[MultiFunctionCall]].
    */
  def buildMultiFunctionCall(
    mf: MultiFunctionDefinition,
    arguments: Vector[Expression],
    position: Position,
  )(implicit reporter: Reporter): Option[Expression] = {
    mf.dispatch(TupleType(arguments.map(_.tpe)), position)
      .map(instance => MultiFunctionCall(instance, arguments, position))
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
