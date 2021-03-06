import { Intersection, IntersectionType } from '../intersections.ts'
import { List, ListType } from '../lists.ts'
import { Map, MapType } from '../maps.ts'
import { Shape, ShapeType } from '../shapes.ts'
import { Sum, SumType } from '../sums.ts'
import { Function, FunctionType } from '../functions.ts'
import { TupleType, Tuple } from '../tuples.ts'
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
    case Kind.Sum:
      return Sum.simplified(substituteMany(assignments, (<SumType> type).types))
    case Kind.Intersection:
      return Intersection.simplified(substituteMany(assignments, (<IntersectionType> type).types))
    case Kind.Tuple:
      return Tuple.type(substituteMany(assignments, (<TupleType> type).types))
    case Kind.Function: {
      const func = <FunctionType> type
      return Function.type(<TupleType> substitute(assignments, func.input), substitute(assignments, func.output))
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
