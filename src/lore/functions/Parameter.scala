package lore.functions

import lore.types.Type

case class Parameter(name: String, tpe: Type) {
  override def toString = s"$name: $tpe"
}
