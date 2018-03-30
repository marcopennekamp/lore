package lore.types

import lore.execution.Context

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
    * @return The set of direct declared subtypes, that is, explicitly declared immediate subtypes, for example
    *         immediate subclasses or direct sub-label types.
    */
  def directDeclaredSubtypes(implicit context: Context): Set[Type]

  /**
    * @return A singleton tuple type enclosing this type, unless this type is already a tuple type.
    */
  def toTuple: TupleType = TupleType(Seq(this))

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
