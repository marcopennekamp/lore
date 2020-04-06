package lore.types

case class SumType private (types: Set[Type]) extends Type {
  override def isAbstract = true
  override def toString: String = s"(${types.mkString(" | ")})"
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
