package lore.compiler.semantics.functions

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.LocalVariable
import lore.compiler.types.Type

class ParameterDefinition(
  val name: String,
  val tpe: Type,
  override val position: Position,
) extends Positioned {
  override def toString = s"$name: $tpe"
  def asLocalVariable: LocalVariable = LocalVariable(name, tpe, isMutable = false)
}
