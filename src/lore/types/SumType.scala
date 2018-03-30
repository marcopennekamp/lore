package lore.types

import lore.execution.Context

case class SumType(types: Set[Type]) extends Type {
  /**
    * Whether any one of the intersection type's types is a subtype of the given candidate type.
    */
  private def isAnyTypeSubtype(candidateSupertype: Type): Boolean = {
    types.exists(tpe => tpe.isSubtype(candidateSupertype))
  }

  // TODO: This may be incomplete. See issue #2 to find out what will give us a proper framework to implement proper typing rules.
  override def isSubtype(other: Type): Boolean = {
    other match {
      case SumType(otherTypes) => types.forall(t => otherTypes.exists(ot => t.isSubtype(ot)))
      case _ => false // TODO: Not true. One rule: A <: C and B <: C => A | B <: C
    }
  }

  override def isSupertype(other: Type): Boolean = {
    other match {
      case SumType(_) => other.isSubtype(this)
      case _ => types.exists(_.isSupertype(other))
    }
  }

  override def directDeclaredSubtypes(implicit context: Context) = types
  override def isAbstract = true
  override def toString = "(" + types.mkString(" | ") + ")"
}
