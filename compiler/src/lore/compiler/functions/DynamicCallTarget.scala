package lore.compiler.functions

import lore.types.Type

case class DynamicCallTarget(override val name: String, override val outputType: Type) extends CallTarget
