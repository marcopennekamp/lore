package lore.compiler.types

// TODO: Move Any to BasicType?

/**
  * The "top" type which is the supertype of all possible types.
  */
object AnyType extends NamedType {
  override val name = "Any"
}
