package lore.compiler.semantics.scopes

import lore.compiler.target.Target

trait Variable {
  def name: String
  def isMutable: Boolean = false

  def asTargetVariable: Target.Variable
}
