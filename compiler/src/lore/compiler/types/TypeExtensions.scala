package lore.compiler.types

import lore.compiler.semantics.Registry

object TypeExtensions {

  implicit class SumTypeExtension(sumType: SumType) {
    /**
      * Joins the sum type, producing a type that is the closest sensible supertype of all individual types of
      * the sum type.
      */
    def join(implicit registry: Registry): Type = {
      if (sumType.parts.size == 1) sumType.parts.head
      else sumType.parts.reduceLeft(LeastUpperBound.lubNoDefaultSum)
    }
  }

}
