package lore.compiler.types

import lore.compiler.semantics.structures.ClassDefinition

class ClassType(
  override val name: String, override val supertype: Option[ClassType], val isAbstract: Boolean,
) extends DeclaredType with DeclaredType.DefinitionProperty[ClassDefinition] {
  /**
    * Only entities of the ownedBy type may own a component of this class type.
    */
  def ownedBy: Option[Type] = definition.ownedBy

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
