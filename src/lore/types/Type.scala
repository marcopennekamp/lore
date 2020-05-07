package lore.types

/**
  * Any Lore type.
  *
  * Note: hashCode should be defined as a val or lazy val to avoid recomputing hashes during the runtime of the
  * compiler. At times, we will hash types heavily, and so fast hash access is important.
  */
trait Type {
  // TODO: As a general performance improvement, we might consider interning types like strings are interned.
  // TODO: Add .product, .sum, and .intersection helper methods to List[Type] for easy construction.

  /**
    * Returns a singleton product type enclosing this type, unless this type is already a product type.
    */
  def toTuple: ProductType = ProductType(List(this))

  /**
    * Whether this type is abstract.
    */
  def isAbstract: Boolean

  /**
    * A pretty string representation of the type.
    */
  def string(parentPrecedence: TypePrecedence): String
  override def toString: String = string(TypePrecedence.Parenthesized)

  // TODO: Replace uses of Subtyping.isSubtype with these operators.
  def <=(rhs: Type): Boolean = Subtyping.isSubtype(this, rhs)
  def <(rhs: Type): Boolean = Subtyping.isStrictSubtype(this, rhs)
}

object Type {
  val predefinedTypes: Map[String, Type] = Map(
    "Any" -> AnyType,
    "Int" -> BasicType.Int,
    "Real" -> BasicType.Real,
    "Boolean" -> BasicType.Boolean,
    "String" -> BasicType.String,
  )
}

sealed abstract class TypePrecedence(protected val value: Int) {
  def <(precedence: TypePrecedence): Boolean = this.value <= precedence.value
}
object TypePrecedence {
  case object Parenthesized extends TypePrecedence(0)
  case object Sum extends TypePrecedence(1)
  case object Intersection extends TypePrecedence(2)
  case object Map extends TypePrecedence(3)
  case object Atom extends TypePrecedence(4)
}

trait OperatorType { self: Type =>
  override def string(parentPrecedence: TypePrecedence): String = {
    val repr = operands.map(_.string(precedence)).mkString(s" $operator ")
    if (precedence < parentPrecedence) s"($repr)" else repr
  }

  protected def precedence: TypePrecedence
  protected def operands: List[Type]
  protected def operator: String
}
