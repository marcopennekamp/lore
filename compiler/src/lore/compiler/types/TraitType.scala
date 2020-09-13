package lore.compiler.types

import lore.compiler.semantics.structures.TraitDefinition

class TraitType(
  override val name: String,
  override val supertypes: Vector[Type],
) extends DeclaredType with DeclaredType.DefinitionProperty[TraitDefinition]
