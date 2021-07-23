package lore.compiler.semantics.structures

import lore.compiler.core.Position
import lore.compiler.types.AliasSchema

class AliasDefinition(
  override val name: String,
  override val schema: AliasSchema,
  override val position: Position,
) extends SchemaDefinition
