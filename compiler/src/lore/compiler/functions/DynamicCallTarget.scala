package lore.compiler.functions

import lore.compiler.types.Type

case class DynamicCallTarget(override val name: String, override val outputType: Type) extends CallTarget
