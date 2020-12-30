package lore.compiler.types

import scala.util.hashing.MurmurHash3

case class SumType private (parts: Set[Type]) extends Type {
  assert(parts.nonEmpty)
  override val hashCode: Int = MurmurHash3.unorderedHash(parts, 0x85f5fe35)
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
  def construct(parts: Vector[Type]): Type = {
    val flattened = parts.flatMap {
      case t: SumType => t.parts
      case t => Vector(t)
    }
    val simplified = Type.mostGeneral(flattened).toSet
    if (simplified.size == 1) simplified.head else new SumType(simplified)
  }

  def construct(parts: Set[Type]): Type = construct(parts.toVector)
}
