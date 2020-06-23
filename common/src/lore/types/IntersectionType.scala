package lore.types

import scala.util.hashing.MurmurHash3

import lore.utils.CollectionExtensions._

// TODO: The construct function ensures that an intersection type contains only unique components. Hence, it is
//       questionable whether types needs to be a set. We would have to be careful with implementing equality
//       and hash codes, but I can see Sets bring about performance problems down the line.

// TODO: Rename all instances of "component" to "part".

case class IntersectionType private (types: Set[Type]) extends Type with OperatorType {
  assert(types.nonEmpty)

  /**
    * An intersection type is abstract if any of its component types are abstract.
    *
    * The reasoning is that the value inhabiting the intersection type will need to have each component type as its
    * type. So there can't be a value that has a type as its exact type (not a subtype) that is abstract. Hence, any
    * one abstract component can turn an intersection type abstract.
    *
    * Note that we consider the idea of augmenting label types here. If the intersection type contains at least one
    * non-label type, we ignore label types in the consideration.
    */
  override val isAbstract: Boolean = {
    val exceptLabels = types.toList.filterNotType[LabelType]

    // If the intersection type consists only of labels, it is NOT an augmented type and thus abstract since
    // non-augmenting label types are abstract.
    if (exceptLabels.isEmpty) {
      true
    } else {
      // In all other cases, we decide abstractness WITHOUT taking augmenting labels into account.
      exceptLabels.exists(_.isAbstract)
    }
  }

  override val isPolymorphic: Boolean = types.exists(_.isPolymorphic)

  override protected def precedence: TypePrecedence = TypePrecedence.Intersection
  override protected def operands: List[Type] = types.toList
  override protected def operator: String = "&"

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

  def construct(types: List[Type]): Type = construct(types.toSet)
}
