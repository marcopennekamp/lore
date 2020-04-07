package lore.types

abstract class BasicType(name: String) extends Type {
  override def isAbstract: Boolean = false
  override def toString: String = name
}

object BasicType {
  case object Real extends BasicType("Real")
  case object Int extends BasicType("Int")
  case object Boolean extends BasicType("Boolean")
  case object String extends BasicType("String")
}
