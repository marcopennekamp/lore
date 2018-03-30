package lore.types

case class LabelType(name: String, supertype: Type) extends Type {
  override def isSubtype(other: Type): Boolean = this == other || supertype.isSubtype(other)
  override def toString = s"$name < $supertype"
}
