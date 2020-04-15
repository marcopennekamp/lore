package lore.functions

import lore.definitions.ParameterDefinition
import lore.types.ProductType

case class LoreFunction(name: String, parameters: List[ParameterDefinition], isAbstract: Boolean) {
  val inputType: ProductType = ProductType(parameters.map(_.tpe))
  override def toString = s"${if (isAbstract) "abstract " else ""}$name(${parameters.mkString(", ")})"
}
