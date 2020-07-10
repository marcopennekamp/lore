package lore.compiler.functions

import lore.compiler.core.Compilation
import lore.compiler.types.{Fit, ProductType, Substitution, Type, TypeVariable}

import scala.util.hashing.MurmurHash3

case class FunctionSignature(name: String, parameters: List[ParameterDefinition], outputType: Type) {
  val inputType: ProductType = ProductType(parameters.map(_.tpe))
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
      val substitutedType = Substitution.substitute(assignments, parameter.tpe)
      new ParameterDefinition(parameter.name, () => Compilation.succeed(substitutedType), parameter.position)
    }
    val substitutedOutputType = Substitution.substitute(assignments, outputType)
    FunctionSignature(name, substitutedParameters, substitutedOutputType)
  }

  /**
    * Whether this function signature is equal in specificity to the given one.
    */
  def isEquallySpecific(other: FunctionSignature): Boolean = {
    // TODO: What about the output type?
    Fit.isEquallySpecific(inputType, other.inputType)
  }
}
