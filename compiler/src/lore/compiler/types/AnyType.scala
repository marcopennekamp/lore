package lore.compiler.types

/**
  * The "top" type which is the supertype of all possible types.
  */
object AnyType extends NamedType {
  override val name = "Any"

  override def string(precedence: TypePrecedence) = "Any"
}
