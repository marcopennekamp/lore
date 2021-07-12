package lore.compiler.semantics.structures

import lore.compiler.core.Position
import lore.compiler.types.Type

class AliasDefinition(
  override val name: String,
  override val tpe: Type,
  override val position: Position
) extends TypeDefinition
