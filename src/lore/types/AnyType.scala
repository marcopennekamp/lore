package lore.types

object AnyType extends Type {
  override def isSubtype(other: Type): Boolean = other == AnyType
  override def isAbstract = true // TODO: Really?
  override def toString = "any"
}
