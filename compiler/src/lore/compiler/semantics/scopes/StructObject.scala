package lore.compiler.semantics.scopes

import lore.compiler.types.StructType

case class StructObject(name: String, tpe: StructType) extends TypedBinding
