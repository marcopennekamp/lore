package lore.compiler.poem

object Poem {
  case class Register(id: Int) extends AnyVal
  case class Location(pc: Int) extends AnyVal

  /**
    * The lowest integer value that may be passed through a `*Const` operation such as `IntConst`.
    */
  val minDirectInteger: Long = Short.MinValue

  /**
    * The highest integer value that may be passed through a `*Const` operation such as `IntConst`.
    */
  val maxDirectInteger: Long = Short.MaxValue
}
