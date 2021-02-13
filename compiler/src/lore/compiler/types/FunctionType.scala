package lore.compiler.types

import scala.util.hashing.MurmurHash3

/**
  * A type that describes multi-functions and anonymous functions as an input/output relation.
  */
case class FunctionType(input: Type, output: Type) extends Type {
  override val hashCode: Int = MurmurHash3.productHash((input, output), 0xf4527105)
}
