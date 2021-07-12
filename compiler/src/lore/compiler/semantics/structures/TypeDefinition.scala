package lore.compiler.semantics.structures

import lore.compiler.core.Positioned
import lore.compiler.types.Type

trait TypeDefinition extends Positioned {
  def name: String
  def tpe: Type
}
