package lore.compiler.semantics.structures

import lore.compiler.core.Position
import lore.compiler.types.TraitType

class LabelDefinition(
  override val name: String,
  override val tpe: TraitType,
  override val position: Position,
) extends DeclaredTypeDefinition {
  override def supertypeDefinition: Option[LabelDefinition] = tpe.supertypes.map(_.definition)
}
