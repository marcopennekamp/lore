package lore.compiler.assembly.optimization

import com.typesafe.scalalogging.Logger
import lore.compiler.core.CompilationException
import lore.compiler.poem.{Poem, PoemInstruction}
import lore.compiler.utils.CollectionExtensions.MapExtension

import scala.collection.immutable.{HashMap, SortedSet}

object RegisterAllocator {

  val logger: Logger = Logger("lore.compiler.assembly.registerAllocation")
  val loggerBlank: Logger = Logger("lore.compiler.assembly.registerAllocation.blank")

  /**
    * Reallocates the registers in the given instructions to reduce the number of used registers. The assignment
    * algorithm is a linear scan, which is a simple but adequate approach. As the expression assembler assigns a new
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
    val liveness = Liveness.compute(instructions)

    logger.whenTraceEnabled {
      Liveness.stringify(liveness).foreach(logger.trace(_))
    }

    assignRegisters(instructions, parameterCount, liveness)
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
    *   - Parameters that are unused won't be contained in the liveness information's end points. These parameters must
    *     be freed immediately, because their assigned register will otherwise be clogged for the entirety of the
    *     function.
    *   - For each instruction at line `line`:
    *     - `use` phase:
    *       - Allocate variables whose lifetime begins at `(line, Use)`.
    *       - Assign `use` variables in the instruction to registers.
    *         - We will later assign `def` variables to registers in a separate step. This allows the register
    *           allocator to produce instructions such as `IntAdd 0 <- 0 1`, where the target register is reusing an
    *           operand register. This is a very common pattern.
    *       - Free variables whose lifetime ends at `(line, Use)`.
    *     - `def` phase:
    *       - Allocate variables whose lifetime begins at `(line, Def)`.
    *       - Assign `def` variables in the instruction to registers.
    *       - Free variables whose lifetime ends at `(line, Def)`.
    */
  private def assignRegisters(
    instructions: Vector[PoemInstruction],
    parameterCount: Int,
    liveness: Liveness,
  ): Vector[PoemInstruction] = {
    var activeAssignments = HashMap[Poem.Register, Poem.Register]()
    val registerPool = new RegisterPool

    def allocateVariable(variable: Poem.Register): Unit = {
      if (activeAssignments.contains(variable)) {
        throw CompilationException(s"Variable `$variable` is already allocated to a register.")
      }
      val register = registerPool.allocate()
      activeAssignments += variable -> register

      logger.trace(s"Allocate register $register for variable $variable")
    }

    def freeVariable(variable: Poem.Register): Unit = {
      if (!activeAssignments.contains(variable)) {
        throw CompilationException(s"Unallocated variable `$variable` cannot be freed.")
      }
      val register = activeAssignments(variable)
      activeAssignments -= variable
      registerPool.free(register)

      logger.trace(s"Free register $register of variable $variable")
    }

    // Allocate registers for the parameters, then free the ones that aren't contained in the liveness information in
    // a second step. Don't free any of the registers until all parameters have been allocated, as an argument value is
    // placed at a fixed register ID at run time.
    for (index <- 0 until parameterCount) {
      val variable = Poem.Register(index)
      allocateVariable(variable)
    }

    for (index <- 0 until parameterCount) {
      val variable = Poem.Register(index)
      if (!liveness.endPoints.contains(variable)) {
        // The parameter is unused. Its register must be freed immediately.
        freeVariable(variable)
      }
    }

    // We can flip the liveness maps to get the exact variables which need to be allocated/freed for each line/phase
    // combination. The "allocate before" map may not contain parameters, because they will be allocated before
    // processing the first instruction.
    val allocateBeforeMap = liveness.startPoints
      .filter { case (variable, _) => variable.id >= parameterCount }
      .invert
    val freeAfterMap = liveness.endPoints.invert

    var resultInstructions = Vector.empty[PoemInstruction]
    for (line <- instructions.indices) {
      var instruction = instructions(line)

      // `use` phase.
      val usePoint = LivenessPoint(line, LivenessPhase.Use)
      allocateBeforeMap.getOrElse(usePoint, Vector.empty).foreach(allocateVariable)
      instruction = PoemInstruction.mapRegisters(instruction, identity, activeAssignments)
      freeAfterMap.getOrElse(usePoint, Vector.empty).foreach(freeVariable)

      // `def` phase.
      val defPoint = LivenessPoint(line, LivenessPhase.Def)
      allocateBeforeMap.getOrElse(defPoint, Vector.empty).foreach(allocateVariable)
      instruction = PoemInstruction.mapRegisters(instruction, activeAssignments, identity)
      freeAfterMap.getOrElse(defPoint, Vector.empty).foreach(freeVariable)

      resultInstructions = resultInstructions :+ instruction
    }

    resultInstructions
  }

}
