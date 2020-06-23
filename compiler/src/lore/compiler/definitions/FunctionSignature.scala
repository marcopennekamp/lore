package lore.compiler.definitions

import lore.types.{Fit, ProductType, Type}

import scala.util.hashing.MurmurHash3

case class FunctionSignature(name: String, parameters: List[ParameterDefinition], outputType: Type) {
  val inputType: ProductType = ProductType(parameters.map(_.tpe))
  override def toString: String = s"$name$inputType: $outputType"
  override val hashCode: Int = MurmurHash3.productHash((name, inputType, outputType))

  /**
    * Whether this function signature is equal in specificity to the given one.
    */
  def isEquallySpecific(other: FunctionSignature): Boolean = {
    // TODO: What about the output type?
    Fit.isEquallySpecific(inputType, other.inputType)
  }
}
