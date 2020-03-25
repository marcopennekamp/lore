package lore.types

import lore.execution.Context

// TODO: Idea... To test (simple) equality of intersection types, we can keep the component types sorted by some
//       stable criterion. This would remove the need to take commutativity into account when comparing for equality.

case class IntersectionType private (types: Set[Type]) extends Type {
  /**
    * Whether any one of the intersection type's types is a subtype of the given candidate type.
    */
  def isComponentTypeSubtypeOf(candidateSupertype: Type): Boolean = {
    types.exists(t => Subtyping.isSubtype(t, candidateSupertype))
  }

  /**
    * An intersection type is abstract if any of its component types are abstract.
    *
    * The reasoning is that the value inhabiting the intersection type will need to have each component type as its
    * type. So there can't be a value that has a type as its exact type (not a subtype) that is abstract. Hence, any
    * one abstract component can turn an intersection type abstract.
    */
  override def isAbstract: Boolean = types.exists(_.isAbstract)

  override def toString: String = "[" + types.mkString(" & ") + "]"
}

object IntersectionType {
  /**
    * Constructs the intersection type from the given types and flattens it if necessary.
    */
  def construct(types: Set[Type]): IntersectionType = {
    new IntersectionType(types.flatMap {
      // If the directly nested type is an intersection type, flatten it.
      case t: IntersectionType => t.types
      case t => Set(t)
    })
  }

  def construct(types: List[Type]): IntersectionType = construct(types.toSet)
}
