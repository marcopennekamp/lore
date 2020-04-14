package lore.types

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
