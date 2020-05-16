package lore.types

trait ClassType extends DeclaredType {
  /**
    * The type a value of this class type must be owned by.
    */
  def ownedBy: Option[Type]

  /**
    * Whether this class type represents an entity.
    */
  def isEntity: Boolean

  /**
    * The list of component types belonging to the entity type. The list is empty if this type is not an entity.
    */
  def componentTypes: List[ComponentType]

  override def supertype: Option[ClassType]
  override def rootSupertype: ClassType = {
    // The compiler might not see this, but of course the root supertype of a class can itself only be a class type.
    super.rootSupertype.asInstanceOf[ClassType]
  }
  override def verbose = s"${if (isAbstract) s"abstract class" else "class"} $toString extends ${supertype.getOrElse(AnyType)}"
}
