package lore.compiler.poem

object Poem {
  case class Register(id: Int) extends AnyVal

  /**
    * A label refers to a specific [[PoemInstruction]] and is used to resolve label locations into absolute locations.
    *
    * @param isPost Post labels aren't resolved to the instruction's location but to the location of the next
    *               instruction. This can be used to jump to the end of a block without knowing the next instruction.
    */
  class Label(val isPost: Boolean = false)

  /**
    * A Location is either an unresolved label or an absolute program counter position. Label locations are resolved by
    * [[lore.compiler.assembly.expressions.LabelResolver]] and turned into absolute locations.
    */
  sealed trait Location
  case class LabelLocation(label: Label) extends Location
  case class AbsoluteLocation(pc: Int) extends Location

  /**
    * The lowest integer value that may be passed through a `*Const` operation such as `IntConst`.
    */
  val minDirectInteger: Long = Short.MinValue

  /**
    * The highest integer value that may be passed through a `*Const` operation such as `IntConst`.
    */
  val maxDirectInteger: Long = Short.MaxValue
}
