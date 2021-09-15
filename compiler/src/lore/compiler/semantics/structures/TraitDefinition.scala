package lore.compiler.semantics.structures

import lore.compiler.core.Position
import lore.compiler.semantics.NamePath
import lore.compiler.types.TraitSchema

class TraitDefinition(
  override val name: NamePath,
  override val schema: TraitSchema,
  override val position: Position,
) extends DeclaredSchemaDefinition
