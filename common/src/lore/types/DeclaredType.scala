package lore.types

/**
  * A declared type as defined by the spec.
  */
trait DeclaredType extends Type {
  /**
    * The name of the declared type.
    */
  def name: String

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

  /**
    * A verbose string representation of the type.
    */
  def verbose: String = toString

  override def string(precedence: TypePrecedence): String = name

  // We define equality of declared types as nominal equality.
  override def equals(obj: Any): Boolean = obj match {
    case rhs: DeclaredType => this.eq(rhs) || name == rhs.name
    case _ => false
  }
  override lazy val hashCode: Int = name.hashCode
}
