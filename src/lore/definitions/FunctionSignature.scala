package lore.definitions

import lore.types.{ProductType, Type}

case class FunctionSignature(name: String, parameters: List[ParameterDefinition], outputType: Type) {
  val inputType: ProductType = ProductType(parameters.map(_.tpe))
  override def toString: String = s"$name$inputType: $outputType"
}
