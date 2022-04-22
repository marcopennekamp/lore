package lore.compiler.assembly.functions

import lore.compiler.assembly.{Chunk, RegisterProvider}
import lore.compiler.core.CompilationException
import lore.compiler.poem.{Poem, PoemInstruction}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.{BasicType, TupleType}

object CondAssembler {

  def generate(
    cond: Expression.Cond,
    caseChunks: Vector[(Chunk, Chunk)],
  )(implicit registerProvider: RegisterProvider): Chunk = {
    if (caseChunks.isEmpty) {
      throw CompilationException("`cond` expressions must always have a total case.")
    }

    // We only need a result register if the cond expression is used.
    val regResult = if (cond.isUsed) Some(registerProvider.fresh()) else None

    // If a condition is false, we need to skip that case's body and jump to the next case. The last case has to skip
    // to the `endLabel` instead. If a case body has been executed, we also need to jump to `endLabel` to finish the
    // execution of the expression. Because a `cond` expression is guaranteed to be total, we don't need to add
    // additional handling for non-total `cond`s.
    val caseLabels = cond.cases.map(condCase => new Poem.Label(condCase.condition.position))
    val endLabel = new Poem.Label(cond.position.end)
    val nextLabels = caseLabels.tail :+ endLabel

    val fullCaseChunks = cond.cases.zip(caseChunks).zipWithIndex.map { case ((condCase, (conditionChunk, bodyChunk)), index) =>
      val caseLabel = caseLabels(index)
      val nextLabel = nextLabels(index)

      // If a case is total, we can omit the condition check. Total cases are thus always executed.
      val checkChunk = if (!condCase.isTotalCase) {
        conditionChunk ++ Chunk(
          PoemInstruction.JumpIfFalse(Poem.LabelLocation(nextLabel), conditionChunk.forceResult(condCase.condition.position)),
        )
      } else Chunk.empty

      val assignResultChunk = regResult match {
        case Some(regResult) =>
          bodyChunk.result match {
            case Some(result) => Chunk(PoemInstruction.Assign(regResult, result))
            case None =>
              // If the body doesn't have a result register, we either have a Unit or a Nothing expression. Unit means
              // that we have to assign `unit` to the target register. Nothing means that the execution already
              // stopped, so we don't need to assign anything to `target`.
              condCase.body.tpe match {
                case TupleType.UnitType => Chunk(PoemInstruction.unit(regResult))
                case BasicType.Nothing => Chunk.empty
                case t => throw CompilationException(
                  s"The body of the cond case at ${condCase.body.position} has no result register. We expected either a" +
                    s" result type of Unit or Nothing, but got $t."
                )
              }
          }

        case None => Chunk.empty
      }

      val endBodyChunk = Chunk(
        PoemInstruction.Jump(Poem.LabelLocation(endLabel)),
      )

      val caseChunk = checkChunk ++ bodyChunk ++ assignResultChunk ++ endBodyChunk
      caseChunk.labelFirst(caseLabel)
      caseChunk
    }

    val resultChunk = regResult.map(Chunk(_)).getOrElse(Chunk.empty)
    (Chunk.concat(fullCaseChunks) ++ resultChunk).withPostLabel(endLabel)
  }

}
