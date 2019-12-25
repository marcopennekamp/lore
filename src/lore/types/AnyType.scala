package lore.types

import lore.execution.Context

object AnyType extends Type {
  // TODO: Really?
  //       This should rather be the set of all types which have no supertype, i.e. direct descendants of Any.
  override def directDeclaredSubtypes(implicit context: Context) = Set.empty
  override def isAbstract = true // TODO: Really?
  override def toString = "any"
}
