package lore.types

// TODO: Type equality should be defined as A <= B and B <= A, although this might lead to performance issues,
//       especially with the rampant use of Sets.

trait Type {
  /**
    * Returns a singleton product type enclosing this type, unless this type is already a product type.
    */
  def toTuple: ProductType = ProductType(List(this))

  /**
    *
    * Whether this type is abstract.
    */
  def isAbstract: Boolean

  /**
    *
    * A verbose string representation of the type.
    */
  def verbose: String = toString
}
