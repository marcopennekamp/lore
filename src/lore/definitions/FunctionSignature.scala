package lore.definitions

import lore.types.{ProductType, Type}

case class FunctionSignature(name: String, inputType: ProductType, outputType: Type) {
  override def toString: String = s"$name$inputType: $outputType"
}
