package lore.compiler.types

import lore.compiler.semantics.functions.MultiFunctionDefinition

/**
  * A multi-function type represents a multi-function value. While the type itself has no syntax and cannot be
  * used to type parameters, for example, a multi-function may be passed around as a value to variables and parameters
  * expecting function types.
  */
case class MultiFunctionType(mf: MultiFunctionDefinition) extends NamedType {
  override def name: String = mf.name
}
