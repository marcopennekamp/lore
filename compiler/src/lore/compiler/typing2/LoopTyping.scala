package lore.compiler.typing2

import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.bindings.LocalVariable
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.{Extractor, ForLoop, WhileLoop}
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.semantics.expressions.untyped.UntypedExpression.{UntypedForLoop, UntypedWhileLoop}
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.{Tuple2OptionExtension, VectorExtension}

object LoopTyping {

  def checkOrInfer(
    expression: UntypedWhileLoop,
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit checker: Checker2, registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    checker.check(expression.condition, BasicType.Boolean, context).flatMap { case (typedCondition, context2) =>
      checkOrInferBody(expression.body, expectedType, context2)
        .mapFirst(typedBody => WhileLoop(typedCondition, typedBody, expression.position))
    }
  }

  def checkOrInfer(
    expression: UntypedForLoop,
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit checker: Checker2, registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    inferExtractors(expression, context).flatMap { case (typedExtractors, context2) =>
      checkOrInferBody(expression.body, expectedType, context2).mapFirst { typedBody =>
        ForLoop(typedExtractors, typedBody, expression.position)
      }
    }
  }

  private def checkOrInferBody(
    body: UntypedExpression,
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit checker: Checker2, registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    expectedType match {
      case Some(ListType(elementType)) => checker.check(body, elementType, context)
      case _ => Synthesizer2.infer(body, context)
    }
  }

  private def inferExtractors(
    expression: UntypedForLoop,
    context: InferenceContext,
  )(implicit checker: Checker2, registry: Registry, reporter: Reporter) = {
    expression.extractors.foldSome((Vector.empty[Extractor], context)) {
      case ((typedExtractors, context2), extractor) =>
        Synthesizer2.infer(extractor.collection, context2).flatMap { case (typedCollection, context3) =>
          resolveElementType(typedCollection).map { elementType =>
            val typedVariable = LocalVariable(extractor.variable, elementType)
            (
              typedExtractors :+ Extractor(typedVariable, typedCollection),
              context3.withLocalVariable(typedVariable),
            )
          }
        }
    }
  }

  private def resolveElementType(typedCollection: Expression)(implicit reporter: Reporter): Option[Type] = {
    typedCollection.tpe match {
      case ListType(element) => Some(element)
      case MapType(key, value) => Some(TupleType(key, value))
      case _ =>
        reporter.error(TypingFeedback.Loop.CollectionExpected(typedCollection.tpe, typedCollection))
        None
    }
  }

}
