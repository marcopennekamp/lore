import { Kind } from './kinds.ts'
import { ComponentType, ListType, MapType, ProductType, Type, XaryType } from './types.ts'

/**
 * Checks whether the two types are equal.
 *
 * This function is, crucially, used by all dispatch caches and thus its performance is very important. It is also
 * used during sum/intersection type simplification, whose performance is also important so that complex type
 * construction can run as fast as possible.
 */
export function areEqual(t1: Type, t2: Type): boolean {
  // We are expecting the function to produce positive results more often, by virtue of being used by dispatch caching,
  // so this strict reference check comes before we check for trivial inequality.
  if (t1 === t2) return true

  // If the types have different hashes or different kinds, they cannot possibly be equal.
  if (t1.hash !== t2.hash || t1.kind !== t2.kind) return false

  switch (t1.kind) {
    case Kind.TypeVariable:
      // It has already been established that t1 and t2 are not referentially equal. Type variable equality is
      // referential equality, so at this point we can trivially return false.
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
      // TODO: Can we optimize this? We should take into account that intersection and sum types must exist in their
      //       normal forms, so they will be flattened and each part will be unique. If we sort such types by some
      //       type order, too, we can also use that to walk both arrays once instead of doing nested loops twice.
      //       The normal form could even lead to the result that, if the two types don't have an equal amount of parts,
      //       those parts cannot possibly be equal. Maybe we should try to prove that generally, so that we can be sure
      //       of this property.
      // TODO: Since xary types are currently not interned, the reference equality check at the top will almost always
      //       fail. This leads to the slow check being performed. Would it make sense to intern xary types or does
      //       that create too much overhead by itself?
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
  // TODO: We could probably implement a smarter algorithm that doesn't need to check each pair if we rely on some
  //       type order. Just ordering by kinds would allow us to quickly skip through most entries.
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
