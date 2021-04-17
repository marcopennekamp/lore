package lore.compiler.semantics.scopes

import lore.compiler.phases.transpilation.RuntimeNames
import lore.compiler.target.Target
import lore.compiler.types.Type

/**
  * TODO: Rename this to DeclaredVariable or something to that effect? There is no need to have a separate
  *       GlobalVariable type because the local/global distinction is solely up to the scope a variable resides in.
  */
case class LocalVariable(name: String, tpe: Type, override val isMutable: Boolean) extends TypedVariable {
  override lazy val asTargetVariable: Target.Variable = RuntimeNames.localVariable(name)
}
