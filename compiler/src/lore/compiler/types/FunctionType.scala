package lore.compiler.types

import lore.compiler.core.CompilationException
import lore.compiler.inference.InferenceVariable

import scala.util.hashing.MurmurHash3

/**
  * A type that describes functions as a fixed input/output relation. A function type makes no assumptions about
  * internal dispatch mechanics. A multi-function may as well be viewed through the lens of a function type, while
  * anonymous functions have no dispatch mechanics at all.
  *
  * The input type must always be a tuple type (or an inference variable) so that we can distinguish between functions
  * which take a single tuple argument vs. two distinct arguments. That is, `((A, B)) => C` vs. `(A, B) => C`.
  */
case class FunctionType(input: Type, output: Type) extends Type {
  // This seems like the best way to (softly) enforce the need for tuple types here. A sum type would be great, but
  // isn't very comfortable in Scala 2.
  if (!input.isInstanceOf[TupleType] && !input.isInstanceOf[InferenceVariable]) {
    throw CompilationException(s"A function type's input type must either be a tuple type or an inference variable. Actual type: $input.")
  }

  lazy val inputTuple: TupleType = input.asInstanceOf[TupleType]

  lazy val parameters: Vector[Type] = input match {
    case TupleType(elements) => elements
    case _ => throw CompilationException(s"Can't retrieve parameters from non-tuple input type. Input type: $input.")
  }

  override val hashCode: Int = MurmurHash3.productHash((input, output), 0xf4527105)
}
