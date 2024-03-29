package lore.compiler.types

import lore.compiler.semantics.functions.FunctionIdentity

import scala.util.hashing.MurmurHash3

/**
  * A type that describes functions as a fixed input/output relation. A function type makes no assumptions about
  * internal dispatch mechanics. A multi-function may as well be viewed through the lens of a function type, while
  * lambda functions have no dispatch mechanics at all.
  *
  * The input type must always be a tuple type so that we can distinguish between functions which take a single tuple
  * argument vs. two distinct arguments. That is, `((A, B)) => C` vs. `(A, B) => C`.
  */
case class FunctionType(input: TupleType, output: Type) extends Type {
  lazy val identity: FunctionIdentity = new FunctionIdentity {
    override def typeParameters: Vector[TypeVariable] = Vector.empty
    override def parameterTypes: Vector[Type] = input.elements
    override def outputType: Type = output
    override def asFunctionType: FunctionType = FunctionType.this
  }

  def arity: Int = identity.arity

  override val hashCode: Int = MurmurHash3.productHash((input, output), 0xf4527105)
}
