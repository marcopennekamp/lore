package lore.types

/**
  * The "bottom" type which is the subtype of all types.
  */
object NothingType extends Type {
  // TODO: How does Nothing interact with abstract subtypes, totality constraint, etc.?
  override def isAbstract = true // TODO: Is this correct?
  override def string(precedence: TypePrecedence) = "Nothing"
}
