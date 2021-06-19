package lore.compiler.types

import lore.compiler.core.CompilationException
import lore.compiler.utils.CollectionExtensions.VectorExtension

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
    * intersection type has only one part, that type is returned instead. If the intersection type contains
    * multiple shape types, they are combined into a single shape type.
    *
    * We also apply the following simplification: In an intersection type A & B & ..., if A < B, then B can
    * be dropped.
    *
    * The resulting flattened normal form is a requirement for subtyping to work correctly.
    */
  def construct(parts: Vector[Type]): Type = {
    val flattened = parts.flatMap {
      case t: IntersectionType => t.parts
      case t => Vector(t)
    }

    // TODO: Can't tuple types be combined, too?
    //          (A, B) & (C, D) = (A & C, B & D)
    //       In general, normalize covariant and contravariant types: https://dotty.epfl.ch/docs/reference/new-types/intersection-types-spec.html

    val (noShapes, shapes) = flattened.separateByType[ShapeType]
    val shapesCombined = if (shapes.length > 1) {
      noShapes :+ ShapeType.combine(shapes)
    } else flattened

    val simplified = Type.mostSpecific(shapesCombined).toSet
    if (simplified.size == 1) simplified.head else new IntersectionType(simplified)
  }

  def construct(parts: Set[Type]): Type = construct(parts.toVector)
}
