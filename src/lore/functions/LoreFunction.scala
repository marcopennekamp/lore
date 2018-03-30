package lore.functions

import lore.types.TupleType

case class LoreFunction(name: String, parameters: Seq[Parameter], isAbstract: Boolean) {
  val inputType: TupleType = TupleType(parameters.map(_.tpe))
  override def toString = s"${if (isAbstract) "abstract " else ""}$name(${parameters.mkString(", ")})"
}
