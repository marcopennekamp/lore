package lore.compiler.poem

object Poem {
  case class Register(id: Int) extends AnyVal
  case class Location(pc: Int) extends AnyVal
}
