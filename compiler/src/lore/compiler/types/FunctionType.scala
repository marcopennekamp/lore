package lore.compiler.types

import scala.util.hashing.MurmurHash3

/**
  * A type that describes multi-functions and anonymous functions as an input/output relation.
  *
  * The input type must always be a tuple type so that we can distinguish between functions which take a single tuple
  * argument vs. two distinct arguments. That is, `((A, B)) => C` vs. `(A, B) => C`.
  *
  * TODO: It's bothering me slightly that the input type has to be a tuple type. I'm tempted to lift this
  *       "restriction", but there is one good argument for it: If FunctionType was Type => Type, the call syntax
  *       wouldn't mirror this. For example, take a function `f: (Int, Int) => Int`. If we have a value
  *       `v: (Int, Int)`, we should be able to call `f` with `v`. But how? `f(v)` would mean that an argument is
  *       missing. `f v` would break even more assumptions. So it seems that for now the input type does indeed have
  *       to be a tuple type.
  */
case class FunctionType(input: ProductType, output: Type) extends Type {
  override val hashCode: Int = MurmurHash3.productHash((input, output), 0xf4527105)
}
