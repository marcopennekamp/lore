package lore.compiler.assembly.optimization

import lore.compiler.poem.PoemInstruction.DefUseInfo
import lore.compiler.poem.{Poem, PoemInstruction}

import scala.collection.immutable.{HashMap, SortedSet}
import scala.collection.mutable

object RegisterAllocator {

  /**
    * Reallocates the registers in the given instructions to reduce the number of used registers. The assignment
    * algorithm is a linear scan, which is a simple but adequate approach. As the assembly visitor assigns a new
    * temporary index to every expression result, even a simple algorithm can achieve a good compression factor.
    *
    * To reduce confusion between unoptimized and optimized registers, we'll call the former "variables".
    *
    * The first `parameterCount` variables must be allocated to the first registers, because this is where the VM
    * places argument values after a function is called. The registers may be reallocated later if they are freed.
    *
    * Because the VM offers an unlimited number of registers, we don't have to implement variable spills.
    */
  def optimize(instructions: Vector[PoemInstruction], parameterCount: Int): Vector[PoemInstruction] = {
    val defUseInfos = instructions.map(PoemInstruction.defUseInfo)

    val liveness = computeLiveness(instructions, defUseInfos)
    println("Live-in / live-out sets:")
    for (line <- instructions.indices) {
      println(s"$line: {${liveness.liveInSets(line).mkString(", ")}} {${liveness.liveOutSets(line).mkString(", ")}}")
    }
    println()

    assignRegisters(instructions, parameterCount, defUseInfos, liveness)
  }

  /**
    * The live-in and live-out sets can be accessed by index for the ith instruction. The live-in set determines which
    * variables are live at the beginning of the instruction and the live-out set determines which variables are live
    * at the end of the instruction.
    */
  private case class LivenessInfo(
    liveInSets: Vector[mutable.HashSet[Poem.Register]],
    liveOutSets: Vector[mutable.HashSet[Poem.Register]],
  )

  /**
    * Computes the liveness information for all instructions by doing backwards live-in/live-out analysis.
    */
  private def computeLiveness(instructions: Vector[PoemInstruction], defUseInfos: Vector[DefUseInfo]): LivenessInfo = {
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

    LivenessInfo(liveInSets, liveOutSets)
  }

  private class RegisterPool {

    /**
      * The ID of the next fresh register, which hasn't been allocated first.
      */
    private var freshId = 0

    /**
      * We're keeping the freed registers sorted so that lower registers are favored and the generated bytecode is more
      * predictable.
      */
    private var freedRegisters = SortedSet.empty[Poem.Register](Ordering.by(_.id))

    def allocate(): Poem.Register = {
      if (freedRegisters.isEmpty) {
        val register = Poem.Register(freshId)
        freshId += 1
        register
      } else {
        val register = freedRegisters.head
        freedRegisters = freedRegisters.tail
        register
      }
    }

    def free(register: Poem.Register): Unit = {
      freedRegisters += register
    }

  }

  /**
    * Using the supplied liveness information, assigns registers to all variables inside the given instructions.
    *
    * Algorithm:
    *   - Keep a map of all active variable -> register assignments. We have to assign the N parameters at 0, 1, 2, ...
    *     to the first N registers, because the VM places the arguments in the first N registers. These registers can
    *     later be reassigned, but the initial mapping has to be fixed.
    *   - For each instruction:
    *     - If an unallocated variable appears in the live-in set, allocate a register for that variable.
    *     - Assign `use` variables in the instruction to registers.
    *       - We will later assign `def` variables to registers in a separate step. This allows the register
    *         allocator to produce instructions such as `IntAdd 0 <- 0 1`, where the target register is reusing an
    *         operand register. This is a very common pattern.
    *     - If an allocated variable isn't in the live-out set, free the register associated with the variable.
    *     - If an unallocated variable appears in the live-out or `def` set, allocate a register for that variable.
    *     - Assign `def` variables in the instruction to registers.
    */
  private def assignRegisters(
    instructions: Vector[PoemInstruction],
    parameterCount: Int,
    defUseInfos: Vector[DefUseInfo],
    liveness: LivenessInfo,
  ): Vector[PoemInstruction] = {
    var activeAssignments = HashMap[Poem.Register, Poem.Register]()
    val registerPool = new RegisterPool

    def allocateVariableIfUnallocated(variable: Poem.Register): Unit = {
      if (!activeAssignments.contains(variable)) {
        activeAssignments += variable -> registerPool.allocate()
      }
    }

    // Allocate registers for the parameters.
    for (index <- 0 until parameterCount) {
      activeAssignments += Poem.Register(index) -> registerPool.allocate()
    }

    var resultInstructions = Vector.empty[PoemInstruction]
    for (line <- instructions.indices) {
      var instruction = instructions(line)
      val liveIn = liveness.liveInSets(line)
      val liveOut = liveness.liveOutSets(line)
      val defUseInfo = defUseInfos(line)

      liveIn.foreach(allocateVariableIfUnallocated)
      instruction = PoemInstruction.mapRegisters(instruction, identity, activeAssignments)

      activeAssignments
        .filter { case (variable, _) => !liveOut.contains(variable) }
        .foreach { case (variable, register) =>
          activeAssignments -= variable
          registerPool.free(register)
        }

      defUseInfo.definitions.foreach(allocateVariableIfUnallocated)
      liveOut.foreach(allocateVariableIfUnallocated)

      resultInstructions = resultInstructions :+ PoemInstruction.mapRegisters(instruction, activeAssignments, identity)
    }

    resultInstructions
  }

}
