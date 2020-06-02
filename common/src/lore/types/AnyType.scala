package lore.types

/**
  * The "top" type which is the supertype of all possible types.
  */
object AnyType extends NamedType {
  override val name = "Any"

  /**
    * The Any type is abstract because, while all values have the type Any, the type doesn't define any value itself.
    */
  override val isAbstract = true
  override val isParametric = false
  override def string(precedence: TypePrecedence) = "Any"
}
