package lore.compiler.semantics.functions

import lore.compiler.core.{Position, Positioned}
import lore.compiler.phases.transpilation.RuntimeNames
import lore.compiler.semantics.scopes.LocalVariable
import lore.compiler.target.Target
import lore.compiler.types.Type

case class ParameterDefinition(
  name: String,
  tpe: Type,
  override val position: Position,
) extends Positioned {
  override def toString = s"$name: $tpe"
  def asLocalVariable: LocalVariable = LocalVariable(name, tpe, isMutable = false)
  lazy val asTargetParameter: Target.Parameter = RuntimeNames.localVariable(name).asParameter
}
