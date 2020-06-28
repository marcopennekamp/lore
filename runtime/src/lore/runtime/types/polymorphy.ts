import { ListType, MapType, ProductType, Type} from './types.ts'
import { Kind } from './kinds.ts'

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
    case Kind.Class:
    case Kind.Label:
      return false // TODO: Change this once we allow type parameters for classes and labels.
    case Kind.Intersection:
    case Kind.Sum:
    case Kind.Product: {
      const xary = <ProductType> type
      const types = xary.types
      for (let i = 0; i < types.length; i += 1) {
        if (isPolymorphic(types[i])) return true
      }
      return false
    }
    case Kind.Component:
      return false // TODO: Change this once we allow type parameters for classes and labels?
    case Kind.List:
      return isPolymorphic((<ListType> type).element)
    case Kind.Map:
      return isPolymorphic((<MapType> type).key) || isPolymorphic((<MapType> type).value)
  }
  return false
}
