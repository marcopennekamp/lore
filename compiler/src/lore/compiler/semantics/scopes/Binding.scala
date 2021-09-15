package lore.compiler.semantics.scopes

import lore.compiler.target.TargetRepresentable

/**
  * A binding is any named entity used in an expression context. For example, variables and multi-functions are both
  * bindings.
  */
trait Binding extends TargetRepresentable {
  def isMutable: Boolean = false
}
