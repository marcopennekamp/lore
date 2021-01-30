package lore.compiler.semantics.functions

import lore.compiler.core.{Position, Positioned}
import lore.compiler.types._

import scala.util.hashing.MurmurHash3

case class FunctionSignature(
  name: String,
  parameters: Vector[ParameterDefinition],
  outputType: Type,
  override val position: Position
) extends Positioned {
  val inputType: ProductType = ProductType(parameters.map(_.tpe))
  val isPolymorphic: Boolean = Type.isPolymorphic(inputType)
  val isMonomorphic: Boolean = !isPolymorphic
  val arity: Int = parameters.size
  override def toString: String = s"$name$inputType: $outputType"
  override val hashCode: Int = MurmurHash3.productHash((name, inputType, outputType))

  /**
    * Substitutes the given type variable assignments into the parameter types and output type and returns
    * a new function signature.
    */
  def substitute(assignments: TypeVariable.Assignments): FunctionSignature = {
    if (assignments.isEmpty) return this

    val substitutedParameters = parameters.map { parameter =>
      val substitutedType = Type.substitute(assignments, parameter.tpe)
      ParameterDefinition(parameter.name, substitutedType, parameter.position)
    }
    val substitutedOutputType = Type.substitute(assignments, outputType)
    FunctionSignature(name, substitutedParameters, substitutedOutputType, position)
  }

  /**
    * Whether this function signature is equal in specificity to the given one.
    */
  def isEquallySpecific(other: FunctionSignature): Boolean = {
    // TODO: What about the function's name?
    Fit.isEquallySpecific(inputType, other.inputType)
  }
}
