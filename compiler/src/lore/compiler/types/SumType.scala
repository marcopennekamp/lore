package lore.compiler.types

import scala.util.hashing.MurmurHash3

// TODO: Rename all instances of "component" to "part".

case class SumType private (types: Set[Type]) extends Type {
  assert(types.nonEmpty)
  override val hashCode: Int = MurmurHash3.setHash(types)
}

object SumType {
  /**
    * Constructs the sum type from the given types and flattens it if necessary. If the resulting sum type
    * has only one component, this type is returned instead.
    *
    * We also apply the following simplification: In a sum type A | B | ..., if B < A, then B can be dropped.
    * That is, A already "clears the way" for values of type B to be part of the sum type.
    *
    * The resulting flattened normal form is a requirement for subtyping to work correctly.
    */
  def construct(types: Set[Type]): Type = {
    val flattened = types.flatMap {
      case t: SumType => t.types
      case t => Set(t)
    }
    val simplified = Type.mostGeneral(flattened)

    val sum = new SumType(simplified)
    if (sum.types.size == 1) sum.types.head else sum
  }

  def construct(types: Vector[Type]): Type = construct(types.toSet)
}
