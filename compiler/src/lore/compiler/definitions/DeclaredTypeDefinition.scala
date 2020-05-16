package lore.compiler.definitions

import lore.compiler.types.DeclaredType

/**
  * The definition of a declared type.
  */
trait DeclaredTypeDefinition extends PositionedDefinition {
  def name: String
  def tpe: DeclaredType
  def supertypeDefinition: Option[DeclaredTypeDefinition]
}
