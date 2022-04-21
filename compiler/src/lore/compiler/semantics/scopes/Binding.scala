package lore.compiler.semantics.scopes

/**
  * A binding is any named entity used in an expression context. For example, variables and multi-functions are both
  * bindings.
  */
trait Binding {
  def isMutable: Boolean = false
}
