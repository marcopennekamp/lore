import { Kind } from './kinds.ts'
import { ComponentType, ListType, MapType, ProductType, Type, XaryType } from './types.ts'

/**
 * Checks whether the two types are equal.
 */
export function areEqual(t1: Type, t2: Type): boolean {
  if (t1 === t2) return true

  // If the types have different kinds, they cannot possibly be equal.
  if (t1.kind !== t2.kind) return false

  switch (t1.kind) {
    case Kind.TypeVariable:
      // It has already been established that t1 and t2 are not equal. Type variable equality is referential equality,
      // so at this point we can trivially return false.
      return false
    case Kind.Any:
    case Kind.Nothing:
    case Kind.Real:
    case Kind.Int:
    case Kind.Boolean:
    case Kind.String:
      // These types are equal if their kinds are equal. As this has already been established, we can trivially
      // return true.
      return true
    case Kind.Class:
    case Kind.Label:
      // TODO: Implement
      return false
    case Kind.Intersection:
    case Kind.Sum: {
      // To prove that two sum or intersection types are equal, we find for each part in t1 an equal part in t2
      // and vice versa.
      // TODO: Can we optimize this?
      const types1 = (<XaryType> t1).types
      const types2 = (<XaryType> t2).types
      return hasEqualIn(types1, types2) && hasEqualIn(types2, types1)
    }
    case Kind.Product: {
      const types1 = (<ProductType> t1).types
      const types2 = (<ProductType> t2).types
      if (!(types1.length === types2.length)) return false
      for (let i = 0; i < types1.length; i += 1) {
        const e1 = types1[i]
        const e2 = types2[i]
        if (!areEqual(e1, e2)) return false
      }
      return true
    }
    case Kind.Component:
      return areEqual((<ComponentType> t1).underlying, (<ComponentType> t2).underlying)
    case Kind.List:
      return areEqual((<ListType> t1).element, (<ListType> t2).element)
    case Kind.Map:
      return areEqual((<MapType> t1).key, (<MapType> t2).key) && areEqual((<MapType> t1).value, (<MapType> t2).value)
  }
}

/**
 * Checks whether all types t1 in types1 have one type t2 in types2 with areEqual(t1, t2).
 */
function hasEqualIn(types1: Array<Type>, types2: Array<Type>): boolean {
  for (let i = 0; i < types1.length; i += 1) {
    const t1 = types1[i]
    let found = false

    for (let j = 0; !found && j < types2.length; j += 1) {
      const t2 = types2[j]
      if (areEqual(t1, t2)) found = true
    }

    if (!found) return false
  }
  return true
}
