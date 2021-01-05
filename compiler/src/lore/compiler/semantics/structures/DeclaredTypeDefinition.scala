package lore.compiler.semantics.structures

import lore.compiler.core.Positioned
import lore.compiler.types.DeclaredType

trait DeclaredTypeDefinition extends Positioned {
  def name: String
  def tpe: DeclaredType
}
