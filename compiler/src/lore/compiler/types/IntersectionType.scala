package lore.compiler.types

import scala.util.hashing.MurmurHash3

case class IntersectionType private (parts: Set[Type]) extends Type {
  assert(parts.nonEmpty)
  override val hashCode: Int = MurmurHash3.unorderedHash(parts, 0x74a2317d)
}

object IntersectionType {
  /**
    * Constructs an intersection type from the given types and flattens it if necessary. If the resulting
    * intersection type has only one part, this type is returned instead.
    *
    * We also apply the following simplification: In an intersection type A & B & ..., if A < B, then B can
    * be dropped. This is especially useful to simplify intersection types that contain an entity and a
    * related component type.
    *
    * The resulting flattened normal form is a requirement for subtyping to work correctly.
    */
  def construct(parts: Set[Type]): Type = {
    val flattened = parts.flatMap {
      case t: IntersectionType => t.parts
      case t => Set(t)
    }
    val simplified = Type.mostSpecific(flattened, Subtyping.Default)
    val intersection = new IntersectionType(simplified)
    if (intersection.parts.size == 1) intersection.parts.head else intersection
  }

  def construct(parts: Vector[Type]): Type = construct(parts.toSet)
}
