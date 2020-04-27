package lore.definitions

import lore.compiler.Compilation.C
import lore.compiler.feedback.Position
import lore.compiler.phases.verification.LocalVariable
import lore.types.{Type, TypingDeferred}

class ParameterDefinition(
  val name: String,
  override val typeResolver: () => C[Type],
  override val position: Position,
) extends PositionedDefinition with TypingDeferred[Type] {
  override def toString = s"$name: $tpe"
  def asLocalVariable: LocalVariable = LocalVariable(name, tpe, isMutable = false)
}
