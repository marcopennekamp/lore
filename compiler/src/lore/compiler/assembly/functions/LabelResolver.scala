package lore.compiler.assembly.functions

import lore.compiler.core.CompilationException
import lore.compiler.poem.{Poem, PoemInstruction}

import scala.collection.immutable.HashMap

object LabelResolver {

  /**
    * Resolves all label locations in the given instructions and transforms them into absolute locations. All labels
    * referenced by a jump instruction must be attached to exactly one instruction in `instructions`.
    *
    * As this function produces jumps with absolute locations, the instruction list must be given in its final form.
    * For example, adding a new instruction to the list <i>after</i> absolute locations have been resolved is illegal,
    * as it leads to incorrect absolute locations.
    */
  def resolve(instructions: Vector[PoemInstruction]): Vector[PoemInstruction] = {
    val absoluteLocations = computeAbsoluteLocations(instructions)
    instructions.map {
      case instruction@PoemInstruction.Jump(target) => instruction.copy(target = absoluteLocations.resolve(target))
      case instruction@PoemInstruction.JumpIfFalse(target, _) => instruction.copy(target = absoluteLocations.resolve(target))
      case instruction@PoemInstruction.JumpIfTrue(target, _) => instruction.copy(target = absoluteLocations.resolve(target))
      case instruction => instruction
    }
  }

  /**
    * Computes the absolute locations for all labels in the given instructions. All labels referenced by a jump
    * instruction must be attached to exactly one instruction in `instructions`.
    */
  def computeAbsoluteLocations(instructions: Vector[PoemInstruction]): Poem.AbsoluteLocationMap  = {
    var absoluteLocations = HashMap.empty[Poem.Label, Poem.AbsoluteLocation]

    for (index <- instructions.indices) {
      val instruction = instructions(index)
      instruction.labels.foreach { label =>
        // The label may only refer to a single location.
        absoluteLocations.get(label) match {
          case Some(location) => throw CompilationException(
            s"The label of an instruction at location $index is already defined earlier at location ${location.pc}. Label position: ${label.position}."
          )
          case None =>
        }

        absoluteLocations += label -> Poem.AbsoluteLocation(index)
      }
    }

    absoluteLocations
  }

}
