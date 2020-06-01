package lore.compiler.definitions

import lore.compiler.types.DeclaredTypeSchema

/**
  * The definition of a declared type.
  */
trait DeclaredTypeDefinition extends PositionedDefinition {
  def name: String
  def tpe: DeclaredTypeSchema
  def supertypeDefinition: Option[DeclaredTypeDefinition]
}
