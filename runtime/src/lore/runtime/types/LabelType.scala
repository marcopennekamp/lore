package lore.runtime.types

case class LabelType(
  override val name: String,
  override val supertype: Option[LabelType],
) extends lore.types.LabelType with DeclaredType {
  override def rootSupertype: LabelType = super.rootSupertype.asInstanceOf[LabelType]
}
