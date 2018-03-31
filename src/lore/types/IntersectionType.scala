package lore.types

import lore.execution.Context

case class IntersectionType(types: Set[Type]) extends Type {
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
