package lore.types

import lore.execution.Context

case class SumType private (types: Set[Type]) extends Type {
  override def directDeclaredSubtypes(implicit context: Context) = types
  override def isAbstract = true // TODO: Update according to spec.
  override def toString = "[" + types.mkString(" | ") + "]"
}

object SumType {
  /**
    * Constructs the sum type from the given types and flattens it if necessary.
    */
  def construct(types: Set[Type]): SumType = {
    new SumType(types.flatMap {
      // If the directly nested type is a sum type, flatten it.
      case t: SumType => t.types
      case t => Set(t)
    })
  }
}
