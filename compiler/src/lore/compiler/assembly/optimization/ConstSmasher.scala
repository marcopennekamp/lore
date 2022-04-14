package lore.compiler.assembly.optimization

import lore.compiler.poem.{PoemBooleanValue, PoemInstruction, PoemIntValue}

/**
  * The ConstSmasher converts instructions such as `Const(_, PoemIntValue(15))` to more direct constant instructions
  * such as `IntConst(_, 15)`. This step is necessary due to the ValueAssembler always producing PoemValues.
  *
  * For now, only small integers and booleans are smashed. In the future, more complex expressions may be optimized.
  */
object ConstSmasher {

  def optimize(instructions: Vector[PoemInstruction]): Vector[PoemInstruction] = instructions.map(smash)

  private def smash(instruction: PoemInstruction): PoemInstruction = instruction match {
    case PoemInstruction.Const(target, value) =>
      value match {
        case PoemIntValue(value) => PoemInstruction.IntConst(target, value).preservingLabelsOf(instruction)
        case PoemBooleanValue(value) => PoemInstruction.BooleanConst(target, value).preservingLabelsOf(instruction)
        case _ => instruction
      }

    case _ => instruction
  }

}
