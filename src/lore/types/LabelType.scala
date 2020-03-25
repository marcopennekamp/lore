package lore.types

import lore.execution.Context

case class LabelType(name: String, supertype: Type) extends DeclaredType {
  override def directDeclaredSubtypes(implicit context: Context): Set[Type] = {
    context.types.values.filter(_.isInstanceOf[LabelType]).map(_.asInstanceOf[LabelType]).filter(_.supertype == this).toSet
  }
  override def isAbstract = true // TODO: Really?
  override def toString = s"$name"
  override def verbose = s"$toString < $supertype"
}
