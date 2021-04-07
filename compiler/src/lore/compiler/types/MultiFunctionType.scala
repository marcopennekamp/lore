package lore.compiler.types

import lore.compiler.semantics.functions.MultiFunctionDefinition

import scala.util.hashing.MurmurHash3

/**
  * A multi-function type represents a multi-function value. While the type itself has no syntax and cannot be
  * used to type parameters, for example, a multi-function may be passed around as a value to variables and parameters
  * expecting function types.
  */
case class MultiFunctionType(mf: MultiFunctionDefinition) extends NamedType {
  override def name: String = mf.name
  override val hashCode: Int = MurmurHash3.stringHash(mf.name, 0x38eaf928)
}
