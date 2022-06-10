package lore.compiler.semantics.bindings

import lore.compiler.types.Type

trait TypedTermBinding extends TermBinding {
  def tpe: Type
}
