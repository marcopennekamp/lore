package lore.compiler.types

/**
  * The "bottom" type which is the subtype of all types.
  */
object NothingType extends NamedType {
  override val name = "Nothing"
  // TODO: How does Nothing interact with abstract subtypes, totality constraint, etc.?
  override def string(precedence: TypePrecedence) = "Nothing"
}
