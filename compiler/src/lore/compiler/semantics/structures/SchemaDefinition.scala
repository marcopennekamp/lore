package lore.compiler.semantics.structures

import lore.compiler.semantics.Definition
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.types.TypeSchema

/**
  * A schema definition.
  *
  * The position is restricted to the schema definition's name for better error highlighting and index building.
  */
trait SchemaDefinition extends Definition {
  def schema: TypeSchema
  def localModule: LocalModule
}
