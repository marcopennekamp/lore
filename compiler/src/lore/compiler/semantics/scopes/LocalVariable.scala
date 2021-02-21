package lore.compiler.semantics.scopes

import lore.compiler.phases.transpilation.RuntimeNames
import lore.compiler.target.Target
import lore.compiler.types.Type

case class LocalVariable(name: String, tpe: Type, override val isMutable: Boolean) extends Variable {
  override lazy val asTargetVariable: Target.Variable = RuntimeNames.localVariable(name)
}
