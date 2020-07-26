package lore.compiler.types

import lore.compiler.structures.ClassDefinition

class ClassType(
  override val supertype: Option[ClassType], val ownedByDeferred: Option[OwnedByDeferred], val isAbstract: Boolean
) extends DeclaredType with DeclaredType.DefinitionProperty[ClassDefinition] {
  /**
    * The type an instance of this class type must be owned by.
    */
  def ownedBy: Option[Type] = ownedByDeferred.map(_.tpe)

  /**
    * Whether this class type represents an entity.
    */
  def isEntity: Boolean = this.definition.isEntity

  /**
    * The list of component types belonging to the entity type. The list is empty if this type is not an entity.
    */
  lazy val componentTypes: List[ComponentType] = this.definition.components.map(_.componentType)

  override def rootSupertype: ClassType = {
    // The compiler might not see this, but of course the root supertype of a class can itself only be a class type.
    super.rootSupertype.asInstanceOf[ClassType]
  }
}
