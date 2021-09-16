package lore.compiler.semantics.structures

import lore.compiler.core.Position
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.types.TraitSchema

class TraitDefinition(
  override val name: NamePath,
  override val schema: TraitSchema,
  override val localModule: LocalModule,
  override val position: Position,
) extends DeclaredSchemaDefinition
