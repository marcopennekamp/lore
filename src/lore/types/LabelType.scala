package lore.types

import lore.execution.Context

case class LabelType(name: String, supertype: Type) extends Type {
  override def isSubtype(other: Type): Boolean = {
    other match {
      case LabelType(_, _) => this == other || supertype.isSubtype(other)
      case _ => other.isSupertype(this) || supertype.isSubtype(other)
    }
  }

  override def directDeclaredSubtypes(implicit context: Context) = {
    context.types.values.filter(_.isInstanceOf[LabelType]).map(_.asInstanceOf[LabelType]).filter(_.supertype == this).toSet
  }
  override def isAbstract = true // TODO: Really?
  override def toString = s"$name"
  override def verbose = s"$toString < $supertype"
}
