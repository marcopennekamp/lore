package lore.compiler.types

import lore.compiler.definitions.ClassDefinition
import lore.types.Type

class ClassType(
  override val supertype: Option[ClassType], val ownedByDeferred: Option[OwnedByDeferred], val isAbstract: Boolean
) extends lore.types.ClassType with DeclaredType with DeclaredType.DefinitionProperty[ClassDefinition] {
  override def ownedBy: Option[Type] = ownedByDeferred.map(_.tpe)
  override def isEntity: Boolean = this.definition.isEntity
  override lazy val componentTypes: List[ComponentType] = this.definition.components.map(_.componentType)
  override def rootSupertype: ClassType = super.rootSupertype.asInstanceOf[ClassType]
}
