package lore.runtime.types

case class LabelTypeSchema(
  override val name: String,
  override val superschema: Option[LabelTypeSchema],
) extends lore.types.LabelTypeSchema with DeclaredTypeSchema {
  override def rootSuperschema: LabelTypeSchema = super.rootSuperschema.asInstanceOf[LabelTypeSchema]
}
