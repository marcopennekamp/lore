package lore.compiler.semantics.structures

import lore.compiler.core.Positioned
import lore.compiler.types.{DeclaredType, Type}

trait DeclaredTypeDefinition extends Positioned {
  def name: String
  def tpe: DeclaredType
  def ownedBy: Type
}
