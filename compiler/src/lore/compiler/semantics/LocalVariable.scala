package lore.compiler.semantics

import lore.compiler.phases.transpilation.TranspiledName
import lore.compiler.target.Target.TargetName
import lore.compiler.types.Type

case class LocalVariable(name: String, tpe: Type, isMutable: Boolean) {
  lazy val transpiledName: TargetName = TranspiledName.localVariable(name)
}
