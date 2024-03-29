package lore.compiler.typing

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.typed.Expression.{Block, TupleValue}
import lore.compiler.semantics.expressions.untyped.UntypedExpression.UntypedBlock
import lore.compiler.types.{TupleType, Type}
import lore.compiler.utils.CollectionExtensions.Tuple2OptionExtension

object BlockTyping {

  def checkOrInfer(
    block: UntypedBlock,
    expectedType: Option[Type],
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    if (block.expressions.isEmpty) {
      // An empty block must contain at least one expression: the unit value.
      Some(Block(Vector(TupleValue(Vector.empty, block.position)), block.position), context)
    } else {
      Synthesizer.infer(block.expressions.init, context)
        .flatMap { case (typedExpressions, context2) =>
          (
            expectedType match {
              case Some(expectedType) if expectedType != TupleType.UnitType =>
                Checker.check(block.expressions.last, expectedType, context2)
              case _ => Synthesizer.infer(block.expressions.last, context2)
            }
          ).mapFirst(typedExpressions :+ _)
        }
        .mapFirst { typedExpressions =>
          // For a block expression expected to result in Unit, we have to manually add an implicit unit value if the
          // block's last expression doesn't already result in Unit.
          if (expectedType.contains(TupleType.UnitType) && typedExpressions.last.tpe != TupleType.UnitType) {
            Block(typedExpressions :+ TupleValue(Vector.empty, block.expressions.last.position), block.position)
          } else {
            Block(typedExpressions, block.position)
          }
        }
    }
  }

}
