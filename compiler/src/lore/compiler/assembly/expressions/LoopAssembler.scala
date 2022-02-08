package lore.compiler.assembly.expressions

import lore.compiler.assembly.types.PoemTypeAssembler
import lore.compiler.assembly.{AsmChunk, RegisterProvider}
import lore.compiler.poem.{Poem, PoemInstruction}
import lore.compiler.semantics.expressions.Expression.WhileLoop
import lore.compiler.types.TupleType

object LoopAssembler {

  def generate(
    loop: WhileLoop,
    conditionChunk: AsmChunk,
    bodyChunk: AsmChunk,
  )(implicit registerProvider: RegisterProvider): AsmChunk = {
    val ignoreResult = loop.tpe == TupleType.UnitType
    val target = registerProvider.fresh()

    val initializeTargetInstruction = if (!ignoreResult) {
      PoemInstruction.ListPoly(target, PoemTypeAssembler.generate(loop.tpe))
    } else {
      PoemInstruction.tupleUnit(target)
    }

    // We have to prepare instruction counts because we need mutual relative jump targets from body to condition and
    // from the condition past the body. These counts have to be kept up-to-date with their actual instructions.
    //
    // Jump target example:
    // 0:     <cond 1>
    // 1:     <cond 2>
    // 2:     JumpIfFalse --> 6 = rel +4  (bodyInstructionCount(3) + 1 = 4)
    // 3:     <body 1>
    // 4:     Assign
    // 5:     Jump --> abs 0 = rel -5  (1 - bodyInstructionCount(3) - conditionInstructionCount(3) = -5)
    // 6:     <after loop>
    val bodyInstructionCount = bodyChunk.instructions.length + (if (!ignoreResult) 2 else 1)
    val conditionInstructionCount = conditionChunk.instructions.length + 1

    val conditionInstructions = conditionChunk.instructions ++ Vector(
      // The `+1` is necessary to jump past all body instructions.
      PoemInstruction.JumpIfFalse(
        Poem.Location(bodyInstructionCount + 1),
        conditionChunk.forceResult(loop.condition.position)
      )
    )

    val bodyResultInstructions = if (!ignoreResult) Vector(
      PoemInstruction.Assign(target, bodyChunk.forceResult(loop.body.position)),
    ) else Vector.empty
    val postBodyInstructions = Vector(
      // We have to jump back to the beginning of the loop.
      PoemInstruction.Jump(Poem.Location(1 - bodyInstructionCount - conditionInstructionCount)),
    )
    val bodyInstructions = bodyChunk.instructions ++ bodyResultInstructions ++ postBodyInstructions

    AsmChunk(target, Vector(initializeTargetInstruction) ++ conditionInstructions ++ bodyInstructions)
  }

}
