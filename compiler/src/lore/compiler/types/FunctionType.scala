package lore.compiler.types

import scala.util.hashing.MurmurHash3

/**
  * A type that describes multi-functions and anonymous functions as an input/output relation.
  *
  * TODO: Should we always require functions to have a tuple type as the input type? This would simplify quite a bit
  *       when going from multi-functions to functions.
  */
case class FunctionType(input: Type, output: Type) extends Type {
  lazy val parameters: Vector[Type] = Type.tupled(input).elements

  override val hashCode: Int = MurmurHash3.productHash((input, output), 0xf4527105)
}
