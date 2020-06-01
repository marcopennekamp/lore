package lore.types

import scala.util.hashing.MurmurHash3

/**
  * A declared type, which can either be a class/entity or a label.
  */
trait DeclaredType extends Type {
  /**
    * The schema that this declared type was instantiated from.
    */
  def schema: DeclaredTypeSchema

  /**
    * All type arguments that this declared type has been instantiated with.
    */
  def typeArguments: List[Type]

  /**
    * The supertype of the declared type.
    */
  def supertype: Option[DeclaredType]

  /**
    * The supertype of the declared type that directly inherits from Any, possibly this type itself.
    */
  def rootSupertype: DeclaredType = supertype match {
    case None => this
    case Some(tpe) => tpe.rootSupertype
  }

  override def isAbstract: Boolean = schema.isAbstract

  // TODO: For now. This needs to be set to true for classes with type parameters, of course.
  override def isParametric = false

  /**
    * A verbose string representation of the type.
    */
  def verbose: String = toString

  override def string(precedence: TypePrecedence): String = schema.name

  override def equals(obj: Any): Boolean = obj match {
    case rhs: DeclaredType => this.eq(rhs) || (schema.eq(rhs.schema) && typeArguments == rhs.typeArguments)
    case _ => false
  }
  override lazy val hashCode: Int = MurmurHash3.productHash((schema, typeArguments))
}
