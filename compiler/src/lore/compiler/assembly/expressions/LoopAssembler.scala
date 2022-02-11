package lore.compiler.assembly.expressions

import lore.compiler.assembly.types.PoemTypeAssembler
import lore.compiler.assembly.{AsmChunk, RegisterProvider}
import lore.compiler.poem.{Poem, PoemInstruction}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.types.TupleType

object LoopAssembler {

  def generate(
    loop: Expression.WhileLoop,
    conditionChunk: AsmChunk,
    bodyChunk: AsmChunk,
  )(implicit registerProvider: RegisterProvider): AsmChunk = {
    val ignoreResult = loop.tpe == TupleType.UnitType
    val target = registerProvider.fresh()

    // We need two labels. One label for jumping from the body back to the condition for the next iteration, and one
    // label to skip past the body.
    val conditionLabel = new Poem.Label
    val postBodyLabel = new Poem.Label(isPost = true)

    val initializationChunk = AsmChunk(
      if (!ignoreResult) PoemInstruction.List(target, PoemTypeAssembler.generate(loop.tpe), Vector.empty)
      else PoemInstruction.unit(target)
    )

    val checkConditionChunk = AsmChunk(
      conditionChunk.instructions,
      PoemInstruction.JumpIfFalse(
        Poem.LabelLocation(postBodyLabel),
        conditionChunk.forceResult(loop.condition.position)
      ),
    )

    val fullBodyChunk = bodyChunk ++ AsmChunk(
      if (!ignoreResult) Vector(
        PoemInstruction.ListAppendUntyped(target, target, bodyChunk.forceResult(loop.body.position)),
      ) else Vector.empty,

      // We have to jump back to the beginning of the loop.
      PoemInstruction.Jump(Poem.LabelLocation(conditionLabel)),
    )

    checkConditionChunk.labelFirst(conditionLabel)
    fullBodyChunk.labelLast(postBodyLabel)

    initializationChunk ++ checkConditionChunk ++ fullBodyChunk ++ AsmChunk(target)
  }

}
