package lore.types

import lore.compiler.Registry

import scala.util.hashing.MurmurHash3

case class SumType private (types: Set[Type]) extends Type with OperatorType {
  assert(types.nonEmpty)

  override def isAbstract = true
  override protected def precedence: TypePrecedence = TypePrecedence.Sum
  override protected def operands: List[Type] = types.toList
  override protected def operator: String = "|"
  override val hashCode: Int = MurmurHash3.setHash(types)

  /**
    * Joins the sum type, producing a type that is the closest sensible supertype of all individual types of
    * the sum type.
    */
  def join(implicit registry: Registry): Type = {
    if (types.size == 1) types.head
    else types.reduceLeft(Subtyping.configurableLub(defaultToSum = false))
  }
}

object SumType {
  /**
    * Constructs the sum type from the given types and flattens it if necessary. If the resulting sum type
    * has only one component, this type is returned instead.
    */
  def construct(types: Set[Type]): Type = {
    // TODO: Add a sort of simplification similar to the simplification of intersection types?
    val sum = new SumType(types.flatMap {
      // If the directly nested type is a sum type, flatten it.
      case t: SumType => t.types
      case t => Set(t)
    })
    if (sum.types.size == 1) sum.types.head else sum
  }

  def construct(types: List[Type]): Type = construct(types.toSet)
}
