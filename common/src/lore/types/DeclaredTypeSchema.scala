package lore.types

/**
  * A schema for a declared type.
  *
  * Equality of declared types is based on equality of REFERENCES of declared type schemas. So make sure
  * that each type schema is unique!
  */
trait DeclaredTypeSchema extends TypeSchema[DeclaredType] {
  /**
    * The name of the declared type.
    */
  def name: String

  /**
    * Whether the type instantiated by this schema is abstract.
    */
  def isAbstract: Boolean

  /**
    * The supertype schema of the declared type schema.
    */
  def superschema: Option[DeclaredTypeSchema]

  /**
    * The supertype schema of the declared type schema that directly inherits from Any, possibly this type itself.
    */
  def rootSuperschema: DeclaredTypeSchema = superschema match {
    case None => this
    case Some(schema) => schema.rootSuperschema
  }

  // TODO: Implement equals and hashcode?
}
