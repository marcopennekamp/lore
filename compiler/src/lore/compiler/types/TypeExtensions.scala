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

  implicit class TypeVariableListExtension(variables: Iterable[TypeVariable]) {
    /**
      * Orders the type variables in their order of declaration so that depending variables follow their
      * dependencies.
      */
    def declarationOrder: Vector[TypeVariable] = {
      variables.toVector.sortBy(_.declarationOrder)
    }
  }

}
