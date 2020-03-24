package lore.types

import lore.execution.Context

case class IntersectionType private (types: Set[Type]) extends Type {
  /**
    * Whether any one of the intersection type's types is a subtype of the given candidate type.
    */
  def isComponentTypeSubtypeOf(candidateSupertype: Type): Boolean = {
    types.exists(t => Subtyping.isSubtype(t, candidateSupertype))
  }

  /**
    * The set of direct declared subtypes for an intersection type consists of the combinatorially constructed
    * direct declared subtypes of each component type.
    */
  override def directDeclaredSubtypes(implicit context: Context) = {
    Subtyping.directDeclaredSubtypeCombinations(types.toList).map(_.toSet).map(IntersectionType(_))
  }

  /**
    * An intersection type is abstract if any of its component types are abstract.
    *
    * The reasoning is that the value inhabiting the intersection type will need to have each component type as its
    * type. So there can't be a value that has a type as its exact type (not a subtype) that is abstract. Hence, any
    * one abstract component can turn an intersection type abstract.
    */
  override def isAbstract = types.exists(_.isAbstract)

  override def toString = "[" + types.mkString(" & ") + "]"
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
}
