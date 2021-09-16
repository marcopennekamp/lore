package lore.compiler.semantics.structures

import lore.compiler.core.Positioned
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.types.TypeSchema

/**
  * A schema definition.
  *
  * The position is restricted to the schema definition's name for better error highlighting and index building.
  */
trait SchemaDefinition extends Positioned {
  def name: NamePath
  def schema: TypeSchema
  def localModule: LocalModule
}
