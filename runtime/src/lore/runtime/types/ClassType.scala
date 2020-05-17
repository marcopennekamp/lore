package lore.runtime.types

import lore.types.Type

import scala.scalajs.js.annotation.JSExportTopLevel

case class ClassType(
  override val name: String,
  override val supertype: Option[ClassType],
  override val ownedBy: Option[Type],
  override val isAbstract: Boolean,
  override val isEntity: Boolean,
  override val componentTypes: List[ComponentType],
) extends lore.types.ClassType with DeclaredType {
  override def rootSupertype: ClassType = super.rootSupertype.asInstanceOf[ClassType]
}
