/**
 * In the given type, substitute all occurrences of any given variable with its respective type value.
 */
import { Assignments } from './fit.ts'
import { IntersectionType, ProductType, SumType, Type } from './types.ts'
import { Kind } from './kinds.ts'
import { intersection, list, ListType, map, MapType, product, sum, TypeVariable} from './types.ts'
import { TinyMap } from '../utils/TinyMap.ts'

/**
 * Substitutes the given assignments into the type, leaving the type as is and returning a new type.
 */
export function substitute(assignments: Assignments, type: Type): Type {
  switch (type.kind) {
    case Kind.TypeVariable:
      return TinyMap.get(assignments, <TypeVariable> type) ?? type
    case Kind.Class:
    case Kind.Label:
      return type // TODO: Change this once we allow type parameters for classes and labels.
    case Kind.Intersection:
      // TODO: We need to simplify here, like in IntersectionType.construct.
      return intersection(substituteMany(assignments, (<IntersectionType> type).types))
    case Kind.Sum:
      // TODO: We need to simplify here, like in SumType.construct.
      return sum(substituteMany(assignments, (<SumType> type).types))
    case Kind.Product:
      return product(substituteMany(assignments, (<ProductType> type).types))
    case Kind.Component:
      return type // TODO: Change this once we allow type parameters for classes and labels?
    case Kind.List:
      return list(substitute(assignments, (<ListType> type).element))
    case Kind.Map:
      const m1 = <MapType> type
      return map(substitute(assignments, m1.key), substitute(assignments, m1.value))
    default:
      return type
  }
}

function substituteMany(assignments: Assignments, types: Array<Type>): Array<Type> {
  const result = []
  for (let i = 0; i < types.length; i += 1) {
    result.push(substitute(assignments, types[i]))
  }
  return result
}