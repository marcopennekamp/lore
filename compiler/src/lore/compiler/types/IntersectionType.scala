package lore.compiler.types

import lore.compiler.core.CompilationException

import scala.util.hashing.MurmurHash3

case class IntersectionType private (parts: Set[Type]) extends Type {
  if (parts.isEmpty) {
    throw CompilationException("You may not construct an intersection type with an empty set of parts.")
  }

  override val hashCode: Int = MurmurHash3.unorderedHash(parts, 0x74a2317d)
}

object IntersectionType {
  /**
    * Constructs an intersection type from the given types and flattens it if necessary. If the resulting
    * intersection type has only one part, that type is returned instead. Some types such as shapes and tuples are
    * combined if they occur multiple times.
    *
    * We also apply the following simplification: In an intersection type A & B & ..., if A < B, then B can
    * be dropped. This also leads to the identity for empty intersection types: Any.
    *
    * The resulting flattened normal form is a requirement for subtyping to work correctly.
    */
  def construct(parts: Vector[Type]): Type = {
    if (parts.isEmpty) {
      return BasicType.Any
    }
    Simplification.construct(Kind.Intersection, parts)
  }

  def construct(parts: Set[Type]): Type = construct(parts.toVector)
  def construct(parts: Type*): Type = construct(parts.toVector)
}
