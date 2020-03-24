package lore.types

import lore.execution.Context

object AnyType extends Type {
  // TODO: Really?
  //       This should rather be the set of all types which have no supertype, i.e. direct descendants of Any.
  override def directDeclaredSubtypes(implicit context: Context) = Set.empty

  /**
    * The Any type is abstract because, while all values have the type Any, the type doesn't define any value itself.
    */
  override def isAbstract = true
  override def toString = "any"
}
