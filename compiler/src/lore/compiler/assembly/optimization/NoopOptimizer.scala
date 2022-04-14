package lore.compiler.assembly.optimization

import lore.compiler.poem.PoemInstruction

object NoopOptimizer {

  /**
    * Optimizes:
    *   - `Assign(source, target)`: If `source` and `target` are the same registers, the `Assign` instruction can be
    *     removed. This case is actually quite common after register allocation, especially when a constant is assigned
    *     to a variable (e.g. `IntConst reg0 <- 5, Assign reg0 <- reg0`).
    *   - Jumps: Removes jump instructions that just jump to the next instruction, e.g. `Jump 23` on line 22. Such
    *     instructions are sometimes generated when compiling `if` and `cond` expressions.
    *
    * As an instruction is removed, its labels are added to the next instruction instead. The optimizer expects jump
    * instructions to refer to label locations, not absolute locations.
    */
  def optimize(instructions: Vector[PoemInstruction]): Vector[PoemInstruction] = {
    if (instructions.length < 2) {
      return instructions
    }

    val optimizedInstructions = instructions.sliding(2).flatMap { case Vector(instruction, nextInstruction) =>
      // If `instruction` is deemed to be a no-op, it is removed and its labels are given to `nextInstruction`.
      if (isNoop(instruction, nextInstruction)) {
        nextInstruction.preserveLabelsOf(instruction)
        Vector.empty
      } else Vector(instruction)
    }.toVector

    // The last instruction will never be on the left-hand side of the `sliding` window, so we have to add it manually.
    // This instruction will be a `Return` instruction, so it wouldn't be a no-op in any case.
    optimizedInstructions :+ instructions.last
  }

  private def isNoop(instruction: PoemInstruction, nextInstruction: PoemInstruction): Boolean = instruction match {
    case PoemInstruction.Assign(target, source) => target == source
    case instruction: PoemInstruction.JumpInstruction => nextInstruction.labels.contains(instruction.target.forceLabel)
    case _ => false
  }

}
