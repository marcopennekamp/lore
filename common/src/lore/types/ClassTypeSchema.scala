package lore.types

trait ClassTypeSchema extends DeclaredTypeSchema with TypeSchema[ClassType] {
  // TODO: Add a type variable AND a type argument list. We need the type variables as a sort of parameter declaration
  //       and the type arguments for INSTANCED class types. OR do we add a new type that is an instanced class type?
  //       ARGH.

  /**
    * A potentially parameterized type a value of this class type must be owned by.
    */
  def ownedBy: Option[Type]

  /**
    * Whether a class type instantiated by this schema represents an entity.
    */
  def isEntity: Boolean

  /**
    * The list of potentially parameterized component types belonging to the entity schema. The list is empty if this
    * schema is not an entity schema.
    */
  def componentTypes: List[ComponentType]

  override def superschema: Option[ClassTypeSchema]
  override def rootSuperschema: ClassTypeSchema = super.rootSuperschema.asInstanceOf[ClassTypeSchema]
}
