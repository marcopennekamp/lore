package lore.compiler.semantics

import lore.compiler.phases.transpilation.RuntimeNames
import lore.compiler.target.Target
import lore.compiler.types.Type

case class LocalVariable(name: String, tpe: Type, isMutable: Boolean) {
  lazy val asTargetVariable: Target.Variable = RuntimeNames.localVariable(name)
}
