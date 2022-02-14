package lore.compiler.assembly.expressions

import lore.compiler.assembly.{AsmChunk, RegisterProvider}
import lore.compiler.core.CompilationException
import lore.compiler.poem.{Poem, PoemInstruction}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.{BasicType, TupleType}

object CondAssembler {

  def generate(
    cond: Expression.Cond,
    caseChunks: Vector[(AsmChunk, AsmChunk)],
  )(implicit registerProvider: RegisterProvider): AsmChunk = {
    if (caseChunks.isEmpty) {
      throw CompilationException("`cond` expressions must always have a total case.")
    }

    // TODO (assembly): We can omit the result (and all assignResultChunks) completely if the expression is unused,
    //                  which is still a pending to-do because the analysis has to be implemented first.
    val regResult = registerProvider.fresh()

    // If a condition is false, we need to skip that case's body and jump to the next case. The last case has to skip
    // to the `endLabel` instead. If a case body has been executed, we also need to jump to `endLabel` to finish the
    // execution of the expression. Because a `cond` expression is guaranteed to be total, we don't need to add
    // additional handling for non-total `cond`s.
    val caseLabels = cond.cases.map(_ => new Poem.Label)
    val endLabel = new Poem.Label(isPost = true)
    val nextLabels = caseLabels.tail :+ endLabel

    val fullCaseChunks = cond.cases.zip(caseChunks).zipWithIndex.map { case ((condCase, (conditionChunk, bodyChunk)), index) =>
      val caseLabel = caseLabels(index)
      val nextLabel = nextLabels(index)

      // If a case is total, we can omit the condition check. Total cases are thus always executed.
      val checkChunk = if (!condCase.isTotalCase) {
        conditionChunk ++ AsmChunk(
          PoemInstruction.JumpIfFalse(Poem.LabelLocation(nextLabel), conditionChunk.forceResult(condCase.condition.position)),
        )
      } else AsmChunk.empty

      val assignResultChunk = bodyChunk.result match {
        case Some(result) => AsmChunk(PoemInstruction.Assign(regResult, result))
        case None =>
          // If the body doesn't have a result register, we either have a Unit or a Nothing expression. Unit means that
          // we have to assign `unit` to the target register. Nothing means that the execution already stopped, so we
          // don't need to assign anything to `target`.
          condCase.body.tpe match {
            case TupleType.UnitType => AsmChunk(PoemInstruction.unit(regResult))
            case BasicType.Nothing => AsmChunk.empty
            case t => throw CompilationException(
              s"The body of the cond case at ${condCase.body.position} has no result register. We expected either a" +
                s" result type of Unit or Nothing, but got $t."
            )
          }
      }

      val endBodyChunk = AsmChunk(
        PoemInstruction.Jump(Poem.LabelLocation(endLabel)),
      )

      val caseChunk = checkChunk ++ bodyChunk ++ assignResultChunk ++ endBodyChunk
      caseChunk.labelFirst(caseLabel)
      caseChunk
    }

    AsmChunk.concat(fullCaseChunks) ++ AsmChunk(regResult)
  }

}
