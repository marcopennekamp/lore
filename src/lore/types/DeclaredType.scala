package lore.types

import lore.execution.Context

trait DeclaredType extends Type {
  /**
    * Returns the set of explicitly declared immediate subtypes, for example direct subclasses or direct
    * sub-label types.
    */
  def directDeclaredSubtypes(implicit context: Context): Set[Type]
}
