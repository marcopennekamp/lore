package lore.functions

import lore.types.ProductType

case class LoreFunction(name: String, parameters: List[Parameter], isAbstract: Boolean) {
  val inputType: ProductType = ProductType(parameters.map(_.tpe))
  override def toString = s"${if (isAbstract) "abstract " else ""}$name(${parameters.mkString(", ")})"
}
