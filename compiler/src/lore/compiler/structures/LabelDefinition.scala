package lore.compiler.structures

import lore.compiler.feedback.Position
import lore.compiler.types.LabelType

class LabelDefinition(
  override val name: String,
  override val tpe: LabelType,
  override val position: Position,
) extends DeclaredTypeDefinition {
  override def supertypeDefinition: Option[LabelDefinition] = tpe.supertype.map(_.definition)
}
