package lore.types

sealed abstract class BasicType(name: String) extends Type {
  override def isAbstract: Boolean = false
  override def string(precedence: TypePrecedence): String = name

  override val hashCode: Int = name.hashCode
}

object BasicType {
  case object Real extends BasicType("Real")
  case object Int extends BasicType("Int")
  case object Boolean extends BasicType("Boolean")
  case object String extends BasicType("String")
}
