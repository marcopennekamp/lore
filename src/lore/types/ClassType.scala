package lore.types

import lore.execution.Context

case class ClassType(name: String, supertype: Type, override val isAbstract: Boolean) extends DeclaredType {
  override def directDeclaredSubtypes(implicit context: Context): Set[Type] = {
    context.types.values.filter(_.isInstanceOf[ClassType]).map(_.asInstanceOf[ClassType]).filter(_.supertype == this).toSet
  }
  override def toString = s"$name"
  override def verbose = s"${if (isAbstract) "abstract class" else "class"} $toString < $supertype"
}
