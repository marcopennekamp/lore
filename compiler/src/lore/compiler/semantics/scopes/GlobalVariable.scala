package lore.compiler.semantics.scopes

import lore.compiler.target.Target
import lore.compiler.target.TargetDsl.StringExtension
import lore.compiler.types.Type

case class GlobalVariable(name: String, tpe: Type) extends Variable {
  override def asTargetVariable: Target.Variable = name.asVariable
}
