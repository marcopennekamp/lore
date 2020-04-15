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

object Type {
  val predefinedTypes: Map[String, Type] = Map(
    "Int" -> BasicType.Int,
    "Real" -> BasicType.Real,
    "Boolean" -> BasicType.Boolean,
    "String" -> BasicType.String,
    "Any" -> AnyType,
  )
}
