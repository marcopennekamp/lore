package lore.definitions

import lore.types.{ProductType, Type}

import scala.util.hashing.MurmurHash3

case class FunctionSignature(name: String, parameters: List[ParameterDefinition], outputType: Type) {
  val inputType: ProductType = ProductType(parameters.map(_.tpe))
  override def toString: String = s"$name$inputType: $outputType"
  override val hashCode: Int = MurmurHash3.productHash((name, inputType, outputType))
}
