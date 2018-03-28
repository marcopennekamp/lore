package lore.types

object AnyType extends Type {
  override def isSubtype(other: Type): Boolean = other == AnyType
  override def toString = "any"
}
