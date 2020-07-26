package lore.compiler.types

// TODO: Move Nothing to BasicType?
// TODO: How does Nothing interact with abstract subtypes, totality constraint, etc.?

/**
  * The "bottom" type which is the subtype of all types.
  */
object NothingType extends NamedType {
  override val name = "Nothing"
}
