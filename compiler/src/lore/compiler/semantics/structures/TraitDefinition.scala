package lore.compiler.semantics.structures

import lore.compiler.core.Position
import lore.compiler.types.TraitType

class TraitDefinition(
  override val name: String,
  override val tpe: TraitType,
  override val position: Position,
) extends DeclaredTypeDefinition
