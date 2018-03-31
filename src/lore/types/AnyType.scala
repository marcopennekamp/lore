package lore.types

import lore.execution.Context

object AnyType extends Type {
  override def directDeclaredSubtypes(implicit context: Context) = Set.empty // TODO: Really?
  override def isAbstract = true // TODO: Really?
  override def toString = "any"
}
