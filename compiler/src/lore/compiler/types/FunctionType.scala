package lore.compiler.types

import lore.compiler.core.CompilationException
import lore.compiler.phases.transformation.inference.InferenceVariable

import scala.util.hashing.MurmurHash3

/**
  * A type that describes functions as a fixed input/output relation. A function type makes no assumptions about
  * internal dispatch mechanics. A multi-function may as well be viewed through the lens of a function type, while
  * anonymous functions have no dispatch mechanics at all.
  *
  * The input type must always be a tuple type (or an inference variable) so that we can distinguish between functions
  * which take a single tuple argument vs. two distinct arguments. That is, `((A, B)) => C` vs. `(A, B) => C`.
  *
  * TODO: It's bothering me slightly that the input type has to be a tuple type. I'm tempted to lift this
  *       "restriction", but there is one good argument for it: If FunctionType was Type => Type, the call syntax
  *       wouldn't mirror this. For example, take a function `f: (Int, Int) => Int`. If we have a value
  *       `v: (Int, Int)`, we should be able to call `f` with `v`. But how? `f(v)` would mean that an argument is
  *       missing. `f v` would break even more assumptions. So it seems that for now the input type does indeed have
  *       to be a tuple type.
  */
case class FunctionType(input: Type, output: Type) extends Type {
  // This seems like the best way to (softly) enforce the need for tuple types here. A sum type would be great, but
  // isn't very comfortable in Scala 2.
  if (!input.isInstanceOf[ProductType] && !input.isInstanceOf[InferenceVariable]) {
    throw CompilationException(s"A function type's input type must either be a tuple type or an inference variable. Actual type: $input.")
  }

  lazy val inputTuple: ProductType = input.asInstanceOf[ProductType]

  lazy val parameters: Vector[Type] = input match {
    case ProductType(elements) => elements
    case _ => throw CompilationException(s"Can't retrieve parameters from non-tuple input type. Input type: $input.")
  }

  override val hashCode: Int = MurmurHash3.productHash((input, output), 0xf4527105)
}
