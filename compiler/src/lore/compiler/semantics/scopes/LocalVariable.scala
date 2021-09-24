package lore.compiler.semantics.scopes

import lore.compiler.target.Target
import lore.compiler.transpilation.RuntimeNames
import lore.compiler.types.Type

case class LocalVariable(name: String, tpe: Type, override val isMutable: Boolean) extends TypedBinding {
  lazy val targetVariable: Target.Variable = RuntimeNames.localVariable(name)
}
