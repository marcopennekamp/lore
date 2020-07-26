package lore.compiler.semantics.structures

import lore.compiler.core.Positioned
import lore.compiler.types.DeclaredType

/**
  * The definition of a declared type.
  */
trait DeclaredTypeDefinition extends Positioned {
  def name: String
  def tpe: DeclaredType
  def supertypeDefinition: Option[DeclaredTypeDefinition]
}
