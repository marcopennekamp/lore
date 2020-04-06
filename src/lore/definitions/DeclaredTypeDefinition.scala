package lore.definitions

import lore.types.DeclaredType

/**
  * The definition of a declared type.
  */
trait DeclaredTypeDefinition {
  def name: String
  def tpe: DeclaredType
  def supertypeDefinition: Option[DeclaredTypeDefinition]
}
