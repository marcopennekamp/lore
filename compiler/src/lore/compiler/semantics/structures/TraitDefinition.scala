package lore.compiler.semantics.structures

import lore.compiler.core.Position
import lore.compiler.types.TraitSchema

class TraitDefinition(
  override val name: String,
  override val schema: TraitSchema,
  override val position: Position,
) extends DeclaredSchemaDefinition
