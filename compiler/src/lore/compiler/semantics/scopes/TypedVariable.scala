package lore.compiler.semantics.scopes

import lore.compiler.types.Type

trait TypedVariable extends Variable {
  def tpe: Type
}
