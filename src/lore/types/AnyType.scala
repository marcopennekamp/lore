package lore.types

object AnyType extends Type {
  /**
    * The Any type is abstract because, while all values have the type Any, the type doesn't define any value itself.
    */
  override def isAbstract = true
  override def toString = "any"
}
