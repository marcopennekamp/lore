import { ListType, MapType, Type, TypeVariable, XaryType } from './types.ts'
import { Kind } from './kinds.ts'
import { TinySet } from '../utils/TinySet.ts'

// There is no need to cache whether a type is polymorphic. The important use of this function is to determine
// whether a type is polymorphic at run-time. For the right-hand types in multiple dispatch (where this property
// is actually relevant), that is already determined at compile-time. Hence, the functions isPolymorphic and
// variables are not performance-critical. That should be kept in mind.

export function isPolymorphic(type: Type): boolean {
  switch (type.kind) {
    case Kind.TypeVariable:
      return true

    case Kind.Any:
    case Kind.Nothing:
    case Kind.Real:
    case Kind.Int:
    case Kind.Boolean:
    case Kind.String:
      return false

    case Kind.Struct:
    case Kind.Trait:
      return false // TODO: Change this once we allow type parameters for classes and labels.

    case Kind.Intersection:
    case Kind.Sum:
    case Kind.Product: {
      const xary = <XaryType> type
      const types = xary.types
      for (let i = 0; i < types.length; i += 1) {
        if (isPolymorphic(types[i])) return true
      }
      return false
    }

    case Kind.List:
      return isPolymorphic((<ListType> type).element)
    case Kind.Map:
      return isPolymorphic((<MapType> type).key) || isPolymorphic((<MapType> type).value)

    case Kind.Shape:
      // TODO (shape): Implement.
      return false
  }
  return false
}

export function variables(type: Type): TinySet<TypeVariable> {
  const result: TinySet<TypeVariable> = []

  const traverse = (t: Type) => {
    switch (t.kind) {
      case Kind.TypeVariable:
        TinySet.add(result, <TypeVariable> t)
        break

      case Kind.Struct:
      case Kind.Trait:
        break // TODO: Change this once we allow type parameters for structs and traits.

      case Kind.Intersection:
      case Kind.Sum:
      case Kind.Product: {
        const xary = <XaryType> type
        const types = xary.types
        for (let i = 0; i < types.length; i += 1) {
          traverse(types[i])
        }
        break
      }

      case Kind.List:
        traverse((<ListType> type).element)
        break
      case Kind.Map:
        traverse((<MapType> type).key)
        traverse((<MapType> type).value)
        break

      case Kind.Shape:
        // TODO (shape): Implement.
        return false
    }
  }

  traverse(type)
  return result
}
