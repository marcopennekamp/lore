package lore.types

import lore.execution.Context

case class IntersectionType private (types: Set[Type]) extends Type {
  /**
    * Whether any one of the intersection type's types is a subtype of the given candidate type.
    */
  def isComponentTypeSubtypeOf(candidateSupertype: Type): Boolean = {
    types.exists(t => Subtyping.isSubtype(t, candidateSupertype))
  }

  override def directDeclaredSubtypes(implicit context: Context) = Set.empty // TODO: Really?
  override def isAbstract = false // TODO: Really?
  override def toString = "(" + types.mkString(" & ") + ")"
}

object IntersectionType {
  /**
    * Constructs the intersection type from the given types and it them if necessary.
    */
  def construct(types: Set[Type]): IntersectionType = {
    new IntersectionType(types.flatMap {
      // If the directly nested type is an intersection type, flatten it.
      case t: IntersectionType => t.types
      case t => Set(t)
    })
  }
}
