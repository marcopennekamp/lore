package lore.compiler.types

import scala.util.hashing.MurmurHash3

// TODO: The construct function ensures that an intersection type contains only unique components. Hence, it is
//       questionable whether types needs to be a set. We would have to be careful with implementing equality
//       and hash codes, but I can see Sets bring about performance problems down the line.

// TODO: Rename all instances of "component" to "part".

case class IntersectionType private (types: Set[Type]) extends Type {
  assert(types.nonEmpty)
  override val hashCode: Int = MurmurHash3.setHash(types)
}

object IntersectionType {
  /**
    * Constructs an intersection type from the given types and flattens it if necessary. If the resulting
    * intersection type has only one component, this type is returned instead.
    *
    * We also apply the following simplification: In an intersection type A & B & ..., if A < B, then B can
    * be dropped. This is especially useful to simplify intersection types that contain an entity and a
    * related component type.
    *
    * The resulting flattened normal form is a requirement for subtyping to work correctly.
    */
  def construct(types: Set[Type]): Type = {
    val flattened = types.flatMap {
      case t: IntersectionType => t.types
      case t => Set(t)
    }

    // Remove strict supertypes of other component types.
    val simplified = flattened.filterNot(t => flattened.exists(_ < t))

    val intersection = new IntersectionType(simplified)
    if (intersection.types.size == 1) intersection.types.head else intersection
  }

  def construct(types: Vector[Type]): Type = construct(types.toSet)
}
