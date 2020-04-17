package lore.types

trait Type {
  /**
    * Returns a singleton product type enclosing this type, unless this type is already a product type.
    */
  def toTuple: ProductType = ProductType(List(this))

  /**
    * Whether this type is abstract.
    */
  def isAbstract: Boolean

  /**
    * A verbose string representation of the type.
    */
  def verbose: String = toString

  // TODO: Introduce pretty printing of types that doesn't use parentheses around every single composite type. We can
  //       do this by introducing a "precedence" value that is passed down. Types only need to be parenthesized if their
  //       precedence is lower than that of their enclosing type.
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
