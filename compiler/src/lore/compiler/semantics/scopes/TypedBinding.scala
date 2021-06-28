package lore.compiler.semantics.scopes

import lore.compiler.types.Type

trait TypedBinding extends Binding {
  def tpe: Type
}
