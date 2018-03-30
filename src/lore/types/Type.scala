package lore.types

trait Type {
  /**
    * @return Whether this type is a supertype of the given type.
    */
  def isSupertype(other: Type): Boolean = other.isSubtype(this)

  /**
    * @return Whether this type is a subtype of the given type.
    */
  def isSubtype(other: Type): Boolean

  /**
    *
    * @return Whether this type is abstract.
    */
  def isAbstract: Boolean

  /**
    *
    * @return A verbose string representation of the type.
    */
  def verbose: String = toString
}
