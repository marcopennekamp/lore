package lore.types

trait Type {
  /**
    * Whether this type is a supertype of the given type.
    */
  def isSupertype(other: Type): Boolean = other.isSubtype(this)

  /**
    * Whether this type is a subtype of the given type.
    */
  def isSubtype(other: Type): Boolean

  /**
    *
    * @return A verbose string representation of the type.
    */
  def verbose: String = toString
}
