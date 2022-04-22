package lore.compiler.assembly.optimization

import lore.compiler.poem.PoemInstruction

object NoopOptimizer {

  // TODO: More optimization (as seen in lore.List.repeat):
  //          IntAdd reg4 <- reg2 reg4
  //          Assign reg2 <- reg4
  //       This could be contracted into a single instruction: `IntAdd reg2 <- reg2 reg4`.

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
    // We need to iterate through the instructions in reverse order so that no-op jumps can be optimized away in one
    // go. For example:
    //
    //    Jump <label>
    //    Assign reg0 <- reg0
    //    <label>: Assign reg1 <- reg2
    //
    // The `Jump` should be removed as a no-op, but only when `Assign` is removed first. Going through the instructions
    // in reverse order will do the trick.
    //
    // If `instructions` is empty or only contains a single instruction, the fold will simply return the instruction
    // list unchanged. The last instruction will not be optimized, but since it must be a `Return` instruction, it
    // wouldn't be a no-op in any case.
    instructions.init.foldRight(instructions.lastOption.toVector) { case (instruction, result) =>
      // If `instruction` is deemed to be a no-op, it is removed and its labels are given to `nextInstruction`.
      val nextInstruction = result.head
      if (isNoop(instruction, nextInstruction)) {
        nextInstruction.preserveLabelsOf(instruction)
        result
      } else instruction +: result
    }
  }

  private def isNoop(instruction: PoemInstruction, nextInstruction: PoemInstruction): Boolean = instruction match {
    case PoemInstruction.Assign(target, source) => target == source
    case instruction: PoemInstruction.JumpInstruction => nextInstruction.labels.contains(instruction.target.forceLabel)
    case _ => false
  }

}
