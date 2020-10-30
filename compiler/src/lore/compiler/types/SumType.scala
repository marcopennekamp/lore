package lore.compiler.types

import scala.util.hashing.MurmurHash3

case class SumType private (parts: Set[Type]) extends Type {
  assert(parts.nonEmpty)
  override val hashCode: Int = MurmurHash3.setHash(parts)
}

object SumType {
  /**
    * Constructs the sum type from the given types and flattens it if necessary. If the resulting sum type
    * has only one part, this type is returned instead.
    *
    * We also apply the following simplification: In a sum type A | B | ..., if B < A, then B can be dropped.
    * That is, A already "clears the way" for values of type B to be part of the sum type.
    *
    * The resulting flattened normal form is a requirement for subtyping to work correctly.
    */
  def construct(parts: Set[Type]): Type = {
    val flattened = parts.flatMap {
      case t: SumType => t.parts
      case t => Set(t)
    }
    val simplified = Type.mostGeneral(flattened, Subtyping.Default)
    val sum = new SumType(simplified)
    if (sum.parts.size == 1) sum.parts.head else sum
  }

  def construct(parts: Vector[Type]): Type = construct(parts.toSet)
}
