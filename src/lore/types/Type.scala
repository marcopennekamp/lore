package lore.types

import lore.execution.Context

// TODO: Type equality should be defined as A <= B and B <= A, although this might lead to performance issues,
//       especially with the rampant use of Sets.

trait Type {
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
