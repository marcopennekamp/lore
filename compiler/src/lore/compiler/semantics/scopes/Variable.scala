package lore.compiler.semantics.scopes

import lore.compiler.target.Target
import lore.compiler.types.Type

trait Variable {
  def name: String
  def tpe: Type
  def isMutable: Boolean = false

  def asTargetVariable: Target.Variable
}
