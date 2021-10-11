package lore.compiler.types

import lore.compiler.core.CompilationException

import scala.util.hashing.MurmurHash3

case class SumType private (parts: Set[Type]) extends Type {
  if (parts.isEmpty) {
    throw CompilationException("You may not construct a sum type with an empty set of parts.")
  }

  override val hashCode: Int = MurmurHash3.unorderedHash(parts, 0x85f5fe35)
}

object SumType {
  /**
    * Constructs the sum type from the given types and flattens it if necessary. If the resulting sum type
    * has only one part, this type is returned instead. Some types such as tuples and lists are combined if they occur
    * multiple times.
    *
    * We also apply the following simplification: In a sum type A | B | ..., if B < A, then B can be dropped.
    * That is, A already "clears the way" for values of type B to be part of the sum type. This also leads to the
    * identity for empty sum types: Nothing.
    *
    * The resulting flattened normal form is a requirement for subtyping to work correctly.
    */
  def construct(parts: Vector[Type]): Type = {
    if (parts.isEmpty) {
      return BasicType.Nothing
    }
    Simplification.construct(Kind.Sum, parts)
  }

  def construct(parts: Set[Type]): Type = construct(parts.toVector)
  def construct(parts: Type*): Type = construct(parts.toVector)
}
