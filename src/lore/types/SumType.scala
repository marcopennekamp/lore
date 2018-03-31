package lore.types

import lore.execution.Context

case class SumType(types: Set[Type]) extends Type {
  override def directDeclaredSubtypes(implicit context: Context) = types
  override def isAbstract = true
  override def toString = "(" + types.mkString(" | ") + ")"
}
