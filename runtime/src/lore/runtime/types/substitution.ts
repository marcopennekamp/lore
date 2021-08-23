import { Intersection, IntersectionType } from '../intersections.ts'
import { List, ListType } from '../lists.ts'
import { Map, MapType } from '../maps.ts'
import { Shape, ShapeType } from '../shapes.ts'
import { Struct, StructType } from '../structs.ts'
import { Sum, SumType } from '../sums.ts'
import { Function, FunctionType } from '../functions.ts'
import { Trait, TraitType } from '../traits.ts'
import { TupleType, Tuple } from '../tuples.ts'
import { DeclaredType } from './declared-types.ts'
import { Kind } from './kinds.ts'
import { Assignments, TypeVariable } from './type-variables.ts'
import { Type } from './types.ts'

/**
 * Substitutes the given assignments into the type, leaving the type as is and returning a new type.
 */
export function substitute(assignments: Assignments, type: Type): Type {
  return substitutePolymorphic(type, assignments) ?? type
}

/**
 * Substitutes the given assignments into the type. If no substitutions occur, the function returns `undefined`. This
 * allows it to only allocate new types when a child type was changed.
 */
function substitutePolymorphic(type: Type, assignments: Assignments): Type | undefined {
  switch (type.kind) {
    case Kind.TypeVariable:
      // If the assignments don't contain a type variable with the given index, `undefined` will naturally be returned,
      // which is also the desired behavior: we don't want to substitute a type variable if it cannot be found in the
      // assignments.
      return assignments[(<TypeVariable> type).index]

    case Kind.Trait: {
      const trait = <TraitType> type
      return substituteDeclaredType(trait, assignments, typeArguments => Trait.type(trait.schema, typeArguments))
    }
    case Kind.Struct: {
      const struct = <StructType> type
      return substituteDeclaredType(struct, assignments, typeArguments => Struct.type(struct.schema, typeArguments, struct.openPropertyTypes))
    }

    case Kind.Sum: {
      const parts = substituteMultiplePolymorphic((<SumType> type).types, assignments)
      if (!parts) return undefined
      return Sum.simplified(parts)
    }
    case Kind.Intersection: {
      const parts = substituteMultiplePolymorphic((<IntersectionType> type).types, assignments)
      if (!parts) return undefined
      return Intersection.simplified(parts)
    }
    case Kind.Tuple: {
      const elements = substituteMultiplePolymorphic((<TupleType> type).types, assignments)
      if (!elements) return undefined
      return Tuple.type(elements)
    }

    case Kind.Function: {
      const func = <FunctionType> type
      const input = substitutePolymorphic(func.input, assignments)
      const output = substitutePolymorphic(func.output, assignments)
      if (!(input || output)) return undefined
      return Function.type(<TupleType> input ?? func.input, output ?? func.output)
    }
    case Kind.List: {
      const list = <ListType> type
      const element = substitutePolymorphic(list.element, assignments)
      if (!element) return undefined
      return List.type(element)
    }
    case Kind.Map: {
      const map = <MapType> type
      const key = substitutePolymorphic(map.key, assignments)
      const value = substitutePolymorphic(map.value, assignments)
      if (!(key || value)) return undefined
      return Map.type(key ?? map.key, value ?? map.value)
    }
    case Kind.Shape: {
      const result: { [key: string]: Type } = {}
      const propertyTypes = (<ShapeType> type).propertyTypes

      let substitutionOccurred = false
      for (const name of Object.keys(propertyTypes)) {
        const propertyType = propertyTypes[name]
        const newPropertyType = substitute(assignments, propertyType)
        if (newPropertyType) {
          result[name] = newPropertyType
          substitutionOccurred = true
        } else {
          result[name] = propertyType
        }
      }

      if (!substitutionOccurred) return undefined
      return Shape.type(result)
    }

    default:
      return undefined
  }
}

function substituteDeclaredType(
  dt: DeclaredType,
  assignments: Assignments,
  instantiate: (typeArguments: Assignments) => DeclaredType,
): Type | undefined {
  if (!dt.typeArguments) {
    return undefined
  }

  const typeArguments = substituteMultiplePolymorphic(<Assignments> dt.typeArguments, assignments)
  if (!typeArguments) {
    return undefined
  }

  return instantiate(typeArguments)
}

function substituteMultiplePolymorphic(types: Array<Type>, assignments: Assignments): Array<Type> | undefined {
  const result = new Array(types.length)

  let substitutionOccurred = false
  for (let i = 0; i < types.length; i += 1) {
    const type = types[i]
    const candidate = substitute(assignments, type)
    if (candidate) {
      result[i] = candidate
      substitutionOccurred = true
    } else {
      result[i] = type
    }
  }

  if (!substitutionOccurred) return undefined
  return result
}
