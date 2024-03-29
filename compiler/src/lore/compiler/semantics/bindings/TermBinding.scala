package lore.compiler.semantics.bindings

/**
  * A term is a binding used in an expression context. For example, variables and multi-functions are both considered
  * terms.
  */
trait TermBinding extends Binding {
  def isMutable: Boolean = false
}
