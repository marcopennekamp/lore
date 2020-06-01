package lore.compiler.definitions

import lore.compiler.feedback.Position
import lore.compiler.types.LabelTypeSchema

class LabelDefinition(
  override val name: String,
  override val tpe: LabelTypeSchema,
  override val position: Position,
) extends DeclaredTypeDefinition {
  override def supertypeDefinition: Option[LabelDefinition] = tpe.superschema.map(_.definition)
}
