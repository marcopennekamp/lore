package lore.compiler.semantics.structures

import lore.compiler.core.Positioned
import lore.compiler.types.Type

/**
  * A type definition.
  *
  * The position is restricted to the type definition's name for better error highlighting and index building.
  */
trait TypeDefinition extends Positioned {
  def name: String
  def tpe: Type
}
