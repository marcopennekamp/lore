package lore.definitions

import lore.compiler.Compilation.C
import lore.compiler.feedback.Position
import lore.types.{Type, TypingDeferred}

class ParameterDefinition(
  val name: String,
  override val resolveType: () => C[Type],
  override val position: Position,
) extends PositionedDefinition with TypingDeferred[Type] {
  override def toString = s"$name: $tpe"
}
