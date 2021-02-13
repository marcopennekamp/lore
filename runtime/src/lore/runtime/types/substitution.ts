import { Intersection, IntersectionType } from '../intersections.ts'
import { List, ListType } from '../lists.ts'
import { Map, MapType } from '../maps.ts'
import { Shape, ShapeType } from '../shapes.ts'
import { Sum, SumType } from '../sums.ts'
import { Function, FunctionType } from '../functions.ts'
import { ProductType, Tuple } from '../tuples.ts'
import { TinyMap } from '../utils/TinyMap.ts'
import { Assignments } from './fit.ts'
import { Kind } from './kinds.ts'
import { Type, TypeVariable } from './types.ts'

/**
 * Substitutes the given assignments into the type, leaving the type as is and returning a new type.
 */
export function substitute(assignments: Assignments, type: Type): Type {
  switch (type.kind) {
    case Kind.TypeVariable:
      return TinyMap.get(assignments, <TypeVariable> type) ?? type
    case Kind.Struct:
    case Kind.Trait:
      return type // TODO: Change this once we allow type parameters for classes and labels.
    case Kind.Sum:
      // TODO: We need to simplify here, like in SumType.construct.
      return Sum.type(substituteMany(assignments, (<SumType> type).types))
    case Kind.Intersection:
      // TODO: We need to simplify here, like in IntersectionType.construct.
      return Intersection.type(substituteMany(assignments, (<IntersectionType> type).types))
    case Kind.Product:
      return Tuple.type(substituteMany(assignments, (<ProductType> type).types))
    case Kind.Function: {
      const func = <FunctionType> type
      return Function.type(substitute(assignments, func.input), substitute(assignments, func.output))
    }
    case Kind.List:
      return List.type(substitute(assignments, (<ListType> type).element))
    case Kind.Map: {
      const map = <MapType> type
      return Map.type(substitute(assignments, map.key), substitute(assignments, map.value))
    }
    case Kind.Shape:
      const result: { [key: string]: Type } = { }
      const propertyTypes = (<ShapeType> type).propertyTypes
      for (const name of Object.keys(propertyTypes)) {
        result[name] = substitute(assignments, propertyTypes[name])
      }
      return Shape.type(result)
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
