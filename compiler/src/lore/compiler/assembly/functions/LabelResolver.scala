package lore.compiler.assembly.functions

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.poem.{Poem, PoemInstruction}

import scala.collection.immutable.HashMap

object LabelResolver {

  /**
    * Resolves all label locations in the given instructions and transforms them into absolute locations. All labels
    * referenced by a jump instruction must be attached to exactly one instruction in `instructions`.
    *
    * As this function produces jumps with absolute locations, the instruction list must be given in its final size.
    * For example, adding a new instruction to the beginning of the list <i>after</i> absolute locations have been
    * resolved is illegal, as it leads to incorrect absolute locations.
    *
    * @param position The position of the expression from which the given instructions were generated. This improves
    *                 compilation exception reporting.
    */
  def resolve(instructions: Vector[PoemInstruction], position: Position): Vector[PoemInstruction] = {
    var absoluteLocations = HashMap.empty[Poem.Label, Poem.AbsoluteLocation]

    // Step 1: Collect the locations of all labels.
    for (index <- instructions.indices) {
      val instruction = instructions(index)
      instruction.labels.foreach { label =>
        // The label may only refer to a single location.
        absoluteLocations.get(label) match {
          case Some(location) => throw CompilationException(
            s"The label of an instruction is already defined earlier at location ${location.pc}. Position (estimate): $position."
          )
          case None =>
        }

        val location = if (label.isPost) {
          // We have to ensure that a post label actually refers to a valid instruction.
          if (index + 1 >= instructions.length) {
            throw CompilationException(s"A post label points to an instruction which doesn't exist. Position (estimate): $position.")
          }
          Poem.AbsoluteLocation(index + 1)
        } else {
          Poem.AbsoluteLocation(index)
        }

        absoluteLocations += label -> location
      }
    }

    // Step 2: Modify all jump instructions that contain label locations.
    def resolveLocation(location: Poem.Location): Poem.AbsoluteLocation = location match {
      case Poem.LabelLocation(label) => absoluteLocations.get(label) match {
        case Some(location) => location
        case None => throw CompilationException(s"A label referenced by a jump instruction is not defined. Position (estimate): $position.")
      }
      case location: Poem.AbsoluteLocation => location
    }

    instructions.map {
      case instruction@PoemInstruction.Jump(target) => instruction.copy(target = resolveLocation(target))
      case instruction@PoemInstruction.JumpIfFalse(target, _) => instruction.copy(target = resolveLocation(target))
      case instruction@PoemInstruction.JumpIfTrue(target, _) => instruction.copy(target = resolveLocation(target))
      case instruction => instruction
    }
  }

}
