package lore.compiler.types

import lore.compiler.core.Registry
import lore.types.{SumType, Type}

object TypeExtensions {
  implicit class SumTypeExtension(sumType: SumType) {
    /**
      * Joins the sum type, producing a type that is the closest sensible supertype of all individual types of
      * the sum type.
      */
    def join(implicit registry: Registry): Type = {
      if (sumType.types.size == 1) sumType.types.head
      else sumType.types.reduceLeft(CompilerSubtyping.configurableLub(defaultToSum = false))
    }
  }

  implicit class TypeVariableListExtension(variables: Iterable[lore.types.TypeVariable]) {
    /**
      * Orders the type variables in their order of declaration so that depending variables follow their
      * dependencies.
      */
    def declarationOrder: List[TypeVariable] = {
      variables.toList.map(_.asInstanceOf[TypeVariable]).sortBy(_.declarationOrder)
    }
  }
}
