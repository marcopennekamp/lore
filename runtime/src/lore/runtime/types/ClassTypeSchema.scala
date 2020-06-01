package lore.runtime.types

import lore.types.Type

case class ClassTypeSchema(
  override val name: String,
  override val superschema: Option[ClassTypeSchema],
  override val ownedBy: Option[Type],
  override val isAbstract: Boolean,
  override val isEntity: Boolean,
  override val componentTypes: List[ComponentType],
) extends lore.types.ClassTypeSchema with DeclaredTypeSchema {
  override def rootSuperschema: ClassTypeSchema = super.rootSuperschema.asInstanceOf[ClassTypeSchema]
}
