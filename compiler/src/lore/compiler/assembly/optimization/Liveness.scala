package lore.compiler.assembly.optimization

import lore.compiler.poem.PoemInstruction.DefUseInfo
import lore.compiler.poem.{Poem, PoemInstruction}

import scala.collection.immutable.HashMap
import scala.collection.mutable

/**
  * The liveness phase determines whether a liveness intervals starts/ends at the `use` phase or the `def` phase. An
  * interval `(1, Def) to (2, Use)` and an interval `(2, Def) to (4, Use)` don't overlap, because the former is only
  * live until variables have been used in instruction 2, while the latter only begins to live after the target has
  * been defined in instruction 2.
  */
sealed trait LivenessPhase {
  def <(other: LivenessPhase): Boolean = this == LivenessPhase.Use && other == LivenessPhase.Def
}

object LivenessPhase {
  case object Use extends LivenessPhase
  case object Def extends LivenessPhase
}

case class LivenessPoint(line: Int, phase: LivenessPhase) {
  def <(other: LivenessPoint): Boolean = line < other.line || line == other.line && phase < other.phase
  def <=(other: LivenessPoint): Boolean = this == other || this < other
}

case class LivenessInterval(variable: Poem.Register, from: LivenessPoint, to: LivenessPoint) {
  // TODO (assembly): Do we even need this?
  def overlaps(other: LivenessInterval): Boolean = from <= other.to && other.from <= to
}

case class Liveness(startPoints: Map[Poem.Register, LivenessPoint], endPoints: Map[Poem.Register, LivenessPoint])

object Liveness {

  // TODO (assembly): Maybe we should make DefUseInfo into a lazy val of a PoemInstruction.

  def compute(
    instructions: Vector[PoemInstruction],
    defUseInfos: Vector[DefUseInfo],
  ): Liveness = {
    val (liveInSets, liveOutSets) = computeLiveInOutSets(instructions, defUseInfos)
    computeIntervals(instructions, defUseInfos, liveInSets, liveOutSets)
  }

  // The live-in and live-out sets can be accessed by index for the ith instruction. The live-in set determines which
  // variables are live at the beginning of the instruction and the live-out set determines which variables are live at
  // the end of the instruction.
  private type LiveInSets = Vector[mutable.HashSet[Poem.Register]]
  private type LiveOutSets = Vector[mutable.HashSet[Poem.Register]]

  /**
    * Computes the live-in/live-out sets for all instructions with a backwards algorithm.
    */
  private def computeLiveInOutSets(
    instructions: Vector[PoemInstruction],
    defUseInfos: Vector[DefUseInfo],
  ): (LiveInSets, LiveOutSets) = {
    val liveInSets = instructions.map(_ => mutable.HashSet[Poem.Register]())
    val liveOutSets = instructions.map(_ => mutable.HashSet[Poem.Register]())

    // The live-in set always includes the instructions uses, so we can add them before starting the main loop.
    for (i <- instructions.indices) {
      liveInSets(i).addAll(defUseInfos(i).uses)
    }

    // We compute the live-in and live-out sets with a fixed-point algorithm that iterates until no changes have been
    // made to these sets. Because the sets are only ever growing, the algorithm will eventually terminate, usually
    // within a few iterations.
    //
    // We need a fixed-point algorithm because of backwards jumps. If we have an instruction `Jump 4` in line 12,
    // anything that is live in line 4 will be live in line 12. But when we arrive (backwards) at line 12, we don't
    // know what is live in line 4. This will have to be applied in a second iteration. We potentially need multiple
    // iterations if backwards jumps are "nested".
    var hasChanged = false

    def addVariable(liveSet: mutable.HashSet[Poem.Register], variable: Poem.Register): Unit = {
      val isNew = liveSet.add(variable)
      if (isNew) {
        hasChanged = true
      }
    }

    do {
      hasChanged = false

      for (line <- instructions.indices.reverse) {
        val instruction = instructions(line)
        val defUseInfo = defUseInfos(line)
        val liveIn = liveInSets(line)
        val liveOut = liveOutSets(line)

        // The live-in set is computed as follows: `liveIn[i] = uses[i] union (liveOut[i] without defs[i])`. However,
        // the instruction's uses have already been included.
        for (variable <- liveOut) {
          if (!defUseInfo.definitions.contains(variable)) {
            addVariable(liveIn, variable)
          }
        }

        // The live-out set is computed as follows: `liveOut[i] = union liveIn[s]` of all successor instructions `s`. A
        // "successor instruction" is an instruction that MAY immediately follow `i`. For most instructions,
        // `s = i + 1`. Only `Jump(pc)` instructions have two successors: `i + 1` and `pc`. Because `Return`
        // instructions have no successor, their live-out set must remain empty.
        if (!PoemInstruction.isReturn(instruction)) {
          // The last instruction has no direct successor and thus an empty live-out set.
          if (line + 1 < instructions.length) {
            liveInSets(line + 1).foreach(addVariable(liveOut, _))
          }

          PoemInstruction.getJumpTarget(instruction).foreach { location =>
            liveInSets(location.forcePc).foreach(addVariable(liveOut, _))
          }
        }
      }
    } while (hasChanged)

    (liveInSets, liveOutSets)
  }

  private class LivenessPointAccumulator {
    var entries: Map[Poem.Register, LivenessPoint] = HashMap.empty

    def register(variable: Poem.Register, line: Int, phase: LivenessPhase): Unit = {
      if (!entries.contains(variable)) {
        entries += variable -> LivenessPoint(line, phase)
      }
    }
  }

  /**
    * We compute the start points of the intervals by iterating forward through the instructions, and the end points of
    * the intervals by iterating backward through the instructions.
    */
  private def computeIntervals(
    instructions: Vector[PoemInstruction],
    defUseInfos: Vector[DefUseInfo],
    liveInSets: LiveInSets,
    liveOutSets: LiveOutSets,
  ): Liveness = {
    val startPoints = new LivenessPointAccumulator
    val endPoints = new LivenessPointAccumulator

    for (line <- instructions.indices) {
      liveInSets(line).foreach(startPoints.register(_, line, LivenessPhase.Use))
      liveOutSets(line).foreach(startPoints.register(_, line, LivenessPhase.Def))

      // If a variable is unused, it won't be contained in the live-out set of the instruction where it is defined.
      // This is a problem, because the register allocator still has to assign a register to the variable. Hence, we
      // have to consider the `def` set as well.
      defUseInfos(line).definitions.foreach(startPoints.register(_, line, LivenessPhase.Def))
    }

    for (line <- instructions.indices.reverse) {
      liveOutSets(line).foreach(endPoints.register(_, line, LivenessPhase.Def))
      liveInSets(line).foreach(endPoints.register(_, line, LivenessPhase.Use))
    }

    Liveness(startPoints.entries, endPoints.entries)
  }

}
