package lore.types

import lore.definitions.ClassDefinition

class ClassType(
  override val supertype: Option[ClassType], val ownedBy: Option[OwnedBy], val isAbstract: Boolean
) extends DeclaredType with DeclaredType.DefinitionProperty[ClassDefinition] {
  /**
    * The list of component types belonging to the entity type.
    */
  lazy val componentTypes: List[ComponentType] = this.definition.components.map(_.componentType)
  def isEntity: Boolean = this.definition.isEntity

  override def rootSupertype: ClassType = {
    // The compiler might not see this, but of course the root supertype of a class can itself only be a class type.
    super.rootSupertype.asInstanceOf[ClassType]
  }
  override def verbose = s"${if (isAbstract) s"abstract class" else "class"} $toString extends ${supertype.getOrElse(AnyType)}"
}
