package lore.compiler.semantics.scopes

import lore.compiler.target.Target
import lore.compiler.transpilation.RuntimeNames
import lore.compiler.types.Type

/**
  * A local or global variable.
  */
case class Variable(name: String, tpe: Type, override val isMutable: Boolean) extends TypedBinding {
  lazy val targetVariable: Target.Variable = RuntimeNames.localVariable(name)
}
