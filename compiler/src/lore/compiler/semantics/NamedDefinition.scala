package lore.compiler.semantics

trait NamedDefinition extends Definition {
  def name: NamePath
}
