package lore.functions

import lore.types.TupleType

case class LoreFunction(name: String, parameters: Seq[Parameter]) {
  val inputType: TupleType = TupleType(parameters.map(_.tpe))
  override def toString = s"$name(${parameters.mkString(", ")})"
}
