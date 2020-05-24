package lore.types

/**
  * The "bottom" type which is the subtype of all types.
  */
object NothingType extends Type {
  // TODO: How does Nothing interact with abstract subtypes, totality constraint, etc.?
  override val isAbstract = true // TODO: Is this correct?
  override val isParametric = false
  override def string(precedence: TypePrecedence) = "Nothing"
}
