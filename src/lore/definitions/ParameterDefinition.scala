package lore.definitions

import lore.compiler.{C, Position}
import lore.types.{Type, TypingDeferred}

class ParameterDefinition(
  val name: String,
  override val resolveType: () => C[Type],
  override val position: Position,
) extends Definition with TypingDeferred[Type] {
  override def toString = s"$name: $tpe"
}
