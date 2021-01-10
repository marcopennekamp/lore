package lore.compiler.semantics

import lore.compiler.phases.transpilation.TranspiledName
import lore.compiler.types.Type

case class LocalVariable(name: String, tpe: Type, isMutable: Boolean) {
  lazy val transpiledName: TranspiledName = TranspiledName.localVariable(name)
}
