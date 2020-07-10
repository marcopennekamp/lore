package lore.compiler.types

import lore.compiler.core.Registry

object TypeExtensions {
  implicit class SumTypeExtension(sumType: SumType) {
    /**
      * Joins the sum type, producing a type that is the closest sensible supertype of all individual types of
      * the sum type.
      */
    def join(implicit registry: Registry): Type = {
      if (sumType.types.size == 1) sumType.types.head
      else sumType.types.reduceLeft(LeastUpperBound.configurableLub(defaultToSum = false))
    }
  }

  implicit class TypeVariableListExtension(variables: Iterable[TypeVariable]) {
    /**
      * Orders the type variables in their order of declaration so that depending variables follow their
      * dependencies.
      */
    def declarationOrder: List[TypeVariable] = {
      variables.toList.sortBy(_.declarationOrder)
    }
  }
}
