package lore.compiler.typing

import lore.compiler.feedback.{Feedback, MemoReporter, Reporter, TypingFeedback}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.typed.Expression
import lore.compiler.semantics.expressions.typed.Expression._
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.semantics.expressions.untyped.UntypedExpression._
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.{Tuple2OptionExtension, VectorExtension}

object Checker {

  /**
    * Checks that `expression` has the type `expectedType` (or a subtype thereof) and produces a typed [[Expression]].
    * Any typing errors result in `None`.
    *
    * @param expectedType The type expected from the expression by the surrounding context. It informs certain
    *                     inference decisions.
    */
  def check(
    expression: UntypedExpression,
    expectedType: Type,
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    def fallback = Synthesizer.infer(expression, context)

    // Step 1: Check and/or infer the untyped expression to produce a typed expression.
    val result: Option[InferenceResult] = expression match {
      case UntypedTypeAscription(expression, expectedType, _) => check(expression, expectedType, context)

      case expression@UntypedTupleValue(elements, position) =>
        expectedType match {
          case expectedType@TupleType(elementTypes) =>
            if (elements.length == elementTypes.length) {
              check(elements, elementTypes, context).mapFirst(TupleValue(_, position))
            } else {
              reporter.report(TypingFeedback.Tuple.IncorrectLength(expression, expectedType))
              None
            }

          case _ => fallback
        }

      case expression: UntypedLambdaValue => LambdaTyping.check(expression, expectedType, context)

      case UntypedListValue(elements, position) =>
        expectedType match {
          case ListType(elementType) => check(elements, elementType, context).mapFirst(ListValue(_, position))
          case _ => fallback
        }

      case expression: UntypedShapeValue =>
        expectedType match {
          case shapeType: ShapeType => ShapeTyping.checkOrInfer(expression, Some(shapeType), context)
          case _ => fallback
        }

      case expression: UntypedMultiFunctionCall =>
        MultiFunctionTyping.checkOrInferCall(expression, Some(expectedType), context)

      case expression: UntypedAmbiguousMultiFunctionCall =>
        MultiFunctionTyping.checkOrInferAmbiguousCall(expression, Some(expectedType), context)

      case expression: UntypedConstructorCall =>
        expectedType match {
          case expectedType: DeclaredType => ConstructorTyping.checkOrInferCall(expression, Some(expectedType), context)
          case _ => fallback
        }

      case expression: UntypedValueCall =>
        UniformCallSyntaxTyping.checkOrInferValueCall(expression, Some(expectedType), context)

      case expression: UntypedBindingAccess =>
        BindingAccessTyping.checkOrInfer(expression, Some(expectedType), context).map((_, context))

      case expression: UntypedMemberAccess =>
        UniformCallSyntaxTyping.checkOrInfer(expression, None, Some(expectedType), context)

      case block: UntypedBlock => BlockTyping.checkOrInfer(block, Some(expectedType), context)
      case expression: UntypedCond => CondTyping.checkOrInfer(expression, Some(expectedType), context)
      case expression: UntypedWhileLoop => LoopTyping.checkOrInfer(expression, Some(expectedType), context)
      case expression: UntypedForLoop => LoopTyping.checkOrInfer(expression, Some(expectedType), context)

      case _ => fallback
    }

    result.flatMap { case (typedExpression, _) =>
      Typing.traceExpressionType(typedExpression, "Checked", s" (Expected type: $expectedType.)")

      // Step 2: Check that the typed expression agrees with the expected type.
      Typing.expectType(typedExpression, expectedType).flatMap(_ => result)
    }
  }

  /**
    * Executes [[check]] for all `expressions` and returns a result if typing has succeeded for all expressions.
    */
  def check(
    expressions: Vector[UntypedExpression],
    expectedType: Type,
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResults] = {
    check(expressions, Vector.fill(expressions.length)(expectedType), context)
  }

  /**
    * Executes [[check]] for all pairs of `expressions` and `expectedTypes` and returns a result if typing has
    * succeeded for all expressions.
    */
  def check(
    expressions: Vector[UntypedExpression],
    expectedTypes: Vector[Type],
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResults] = {
    expressions.zip(expectedTypes).foldSomeCollect(context) {
      case (context, (expression, expectedType)) => check(expression, expectedType, context)
    }
  }

  /**
    * Depending on whether `expectedType` is defined, performs [[check]] or [[Synthesizer.infer]].
    */
  // noinspection DuplicatedCode
  def checkOrInfer(
    expression: UntypedExpression,
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = expectedType match {
    case Some(expectedType) => check(expression, expectedType, context)
    case None => Synthesizer.infer(expression, context)
  }

  /**
    * Depending on whether `expectedType` is defined, performs [[check]] or [[Synthesizer.infer]].
    */
  // noinspection DuplicatedCode
  def checkOrInfer(
    expressions: Vector[UntypedExpression],
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResults] = expectedType match {
    case Some(expectedType) => check(expressions, expectedType, context)
    case None => Synthesizer.infer(expressions, context)
  }

  /**
    * Attempts type checking via [[check]], using an internal reporter that accumulates errors, which are then returned
    * separately. `attempt` can be used to try a particular checking path without committing to it.
    */
  def attempt(
    expression: UntypedExpression,
    expectedType: Type,
    context: InferenceContext,
  )(implicit registry: Registry): (Option[InferenceResult], Vector[Feedback]) = {
    implicit val reporter: MemoReporter = MemoReporter()
    (check(expression, expectedType, context), reporter.feedback)
  }

}
