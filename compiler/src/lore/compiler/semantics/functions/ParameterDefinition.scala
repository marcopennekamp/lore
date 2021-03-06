package lore.compiler.semantics.functions

import lore.compiler.core.{Position, Positioned}
import lore.compiler.phases.transpilation.RuntimeNames
import lore.compiler.semantics.scopes.Variable
import lore.compiler.target.Target
import lore.compiler.types.Type

/**
  * A function parameter definition.
  *
  * The position is restricted to the parameter's name for better error highlighting and index building.
  */
case class ParameterDefinition(
  name: String,
  tpe: Type,
  override val position: Position,
) extends Positioned {
  override def toString = s"$name: $tpe"
  def asVariable: Variable = Variable(name, tpe, isMutable = false)
  lazy val asTargetParameter: Target.Parameter = RuntimeNames.localVariable(name).asParameter
}
