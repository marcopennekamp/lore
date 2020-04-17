package lore.types

trait Type {
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
