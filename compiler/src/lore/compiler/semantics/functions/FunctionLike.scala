package lore.compiler.semantics.functions

import lore.compiler.types.{FunctionType, TupleType, Type, TypeVariable}

/**
  * [[FunctionLike]] contains general information about a function's type parameters, parameter types, and its output
  * type. It is a general category for [[FunctionSignature]]s and [[FunctionType]]s. This allows some components of the
  * compiler to work with functions without specifically requiring a signature or type.
  *
  * TODO (multi-import): Find a better name.
  */
trait FunctionLike {
  def typeParameters: Vector[TypeVariable]
  def parameterTypes: Vector[Type]
  def outputType: Type

  lazy val inputType: TupleType = TupleType(typeParameters)

  def isPolymorphic: Boolean = typeParameters.nonEmpty
  def isMonomorphic: Boolean = !isPolymorphic
  def arity: Int = parameterTypes.length

  def asFunctionType: FunctionType = FunctionType(inputType, outputType)

  override def toString: String = s"(${parameterTypes.mkString(", ")}): $outputType"
}
