package lore.compiler.poem

import lore.compiler.core.CompilationException

object Poem {
  case class Register(id: Int) extends AnyVal {
    override def toString: String = s"reg$id"
  }

  object Register {
    /**
      * Returns the maximum ID of the given registers.
      */
    def max(registers: Vector[Register]): Int = registers.map(_.id).max
    def max(registers: Register*): Int = max(registers.toVector)
    def max(reg0: Register, registers: Vector[Register]): Int = max(reg0 +: registers: _*)
    def max(reg0: Register, reg1: Register, registers: Vector[Register]): Int = max(reg0 +: reg1 +: registers: _*)
  }

  /**
    * A label refers to a specific [[PoemInstruction]] and is used to resolve label locations into absolute locations.
    *
    * @param isPost Post labels aren't resolved to the instruction's location but to the location of the next
    *               instruction. This can be used to jump to the end of a block without knowing the next instruction.
    */
  class Label(val isPost: Boolean = false)

  /**
    * A Location is either an unresolved label or an absolute program counter position. Label locations are resolved by
    * [[lore.compiler.assembly.functions.LabelResolver]] and turned into absolute locations.
    */
  sealed trait Location {
    def forcePc: Int = this match {
      case Poem.AbsoluteLocation(pc) => pc
      case Poem.LabelLocation(_) => throw CompilationException("All label locations should have been resolved by now.")
    }
  }

  case class LabelLocation(label: Label) extends Location {
    override def toString: String = s"<$label>"
  }

  case class AbsoluteLocation(pc: Int) extends Location {
    override def toString: String = pc.toString
  }

  /**
    * This can be used to attach sorted property fields to a poem entity.
    */
  trait SortedProperties[A] {
    def properties: Map[String, A]

    private lazy val sortedEntries: Vector[(String, A)] = properties.toVector.sortBy(_._1)
    lazy val sortedNames: Vector[String] = sortedEntries.map(_._1)
    lazy val sortedProperties: Vector[A] = sortedEntries.map(_._2)
  }
}
