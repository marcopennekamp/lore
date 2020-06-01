package lore.types

trait ClassType extends DeclaredType {
  override def schema: ClassTypeSchema
  override def supertype: Option[ClassType]
  override def rootSupertype: ClassType = {
    // The compiler might not see this, but of course the root supertype of a class can itself only be a class type.
    super.rootSupertype.asInstanceOf[ClassType]
  }

  /**
    * The type a value of this class type must be owned by. This is an instantiated version of the schema's ownedBy.
    */
  def ownedBy: Option[Type]

  /**
    * Whether this class represents an entity.
    */
  def isEntity: Boolean = schema.isEntity

  /**
    * The list of component types belonging to the entity. The list is empty if this type is not an entity. This
    * is an instantiated version of the schema's componentTypes.
    */
  def componentTypes: List[ComponentType]

  override def verbose = s"${if (isAbstract) s"abstract class" else "class"} $toString extends ${supertype.getOrElse(AnyType)}"
}
