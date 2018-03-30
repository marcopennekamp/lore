package lore.types

import lore.execution.Context

case class IntersectionType(types: Set[Type]) extends Type {
  /**
    * Whether any one of the intersection type's types is a subtype of the given candidate type.
    */
  private def isAnyTypeSubtype(candidateSupertype: Type): Boolean = {
    types.exists(tpe => tpe.isSubtype(candidateSupertype))
  }

  override def isSubtype(other: Type): Boolean = {
    other match {
      // The intersection type case has to be handled specifically, because we have to deal with subset equalities.
      // TODO: This is obviously quickly cobbled together. The actual intersection subtying semantics might be different.
      case IntersectionType(otherTypes) => otherTypes.forall(candidate => isAnyTypeSubtype(candidate))
      case _ => isAnyTypeSubtype(other)
    }
  }

  override def directDeclaredSubtypes(implicit context: Context) = Set.empty // TODO: Really?
  override def isAbstract = false // TODO: Really?
  override def toString = "(" + types.mkString(" & ") + ")"
}
