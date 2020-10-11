package lore.compiler.types

import lore.compiler.semantics.structures.TraitDefinition

class TraitType(
  override val name: String,
  override val supertypes: Vector[Type],
  override val isOwnable: Boolean,
) extends DeclaredType with DeclaredType.DefinitionProperty[TraitDefinition]
