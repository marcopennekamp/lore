package lore.types

import scala.util.hashing.MurmurHash3

case class SumType private (types: Set[Type]) extends Type with OperatorType {
  override def isAbstract = true
  override protected def precedence: TypePrecedence = TypePrecedence.Sum
  override protected def operands: List[Type] = types.toList
  override protected def operator: String = "|"
  override val hashCode: Int = MurmurHash3.setHash(types)
}

object SumType {
  /**
    * Constructs the sum type from the given types and flattens it if necessary. If the resulting sum type
    * has only one component, this type is returned instead.
    */
  def construct(types: Set[Type]): Type = {
    val sum = new SumType(types.flatMap {
      // If the directly nested type is a sum type, flatten it.
      case t: SumType => t.types
      case t => Set(t)
    })
    if (sum.types.size == 1) sum.types.head else sum
  }

  def construct(types: List[Type]): Type = construct(types.toSet)
}
