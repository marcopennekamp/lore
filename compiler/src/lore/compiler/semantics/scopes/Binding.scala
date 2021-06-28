package lore.compiler.semantics.scopes

import lore.compiler.target.TargetIdentifiable

/**
  * A binding is any named entity used in an expression context. For example, variables and multi-functions are both
  * bindings.
  */
trait Binding extends TargetIdentifiable {
  def name: String
  def isMutable: Boolean = false
}
