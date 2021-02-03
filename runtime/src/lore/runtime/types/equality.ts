import { IntersectionType } from '../intersections.ts'
import { ListType } from '../lists.ts'
import { MapType } from '../maps.ts'
import { ShapeType } from '../shapes.ts'
import { Struct, StructType } from '../structs.ts'
import { SumType } from '../sums.ts'
import { TraitType } from '../traits.ts'
import { ProductType } from '../tuples.ts'
import {
  AnyType, BooleanType, IntType, NothingType, PropertyTypes, RealType, StringType, Type, TypeVariable,
} from './types.ts'

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

  return rules[t1.kind](t1, t2)
}

/**
 * A list of rules to be used for checking type equality. The index of the rule must correspond to the numeric
 * value of the type's kind.
 *
 * A note on performance: Because we already compare the hashes of t1 and t2, it is highly unlikely, though
 * occasionally possible, that a rule is invoked with two types that are actually not equal. In the vast majority
 * of cases, the two types will be equal. We just have to confirm that fact. So in the case of sum types, we could
 * for example check whether both types have the same number of parts. Only then can they be equal. This is not
 * needed to confirm equality, however, and as such shouldn't be part of these rules. In contrast, the check that
 * two product types have the same length is vital, because the verification wouldn't be complete without.
 */
const rules: Array<(t1: any, t2: any) => boolean> = [
  // It has already been established that t1 and t2 are not referentially equal. Type variable equality is
  // referential equality, so at this point we can trivially return false.
  (t1: TypeVariable, t2: TypeVariable) => false,

  // These types are equal if their kinds are equal. As this has already been established, we can trivially
  // return true.
  (t1: AnyType, t2: AnyType) => true,
  (t1: NothingType, t2: NothingType) => true,
  (t1: RealType, t2: RealType) => true,
  (t1: IntType, t2: IntType) => true,
  (t1: BooleanType, t2: BooleanType) => true,
  (t1: StringType, t2: StringType) => true,

  (t1: StructType, t2: StructType) => {
    if (t1.schema === t2.schema) {
      // Structs are only equal if all of their open properties agree as well. This is especially crucial for the
      // dispatch cache, where a single different open property type may change the target function.
      // The equality check assumes that a struct's open property types are either undefined or fully defined, i.e.
      // contain all possible keys. That invariant allows us to iterate through one struct's propertyTypes object and
      // still make sure that all keys are covered. There cannot be an open property type in the other struct that we
      // missed.
      if (t1.propertyTypes) {
        return arePropertyTypesEqualTo(t1.propertyTypes, t2)
      } else if (t2.propertyTypes) {
        return arePropertyTypesEqualTo(t2.propertyTypes, t1)
      }
      return true
    }
    return false
  },
  (t1: TraitType, t2: TraitType) => false, // If the traits are not referentially equal, they cannot be equal.

  // To prove that two sum or intersection types are equal, we find for each part in t1 an equal part in t2
  // and vice versa.
  // TODO: Can we optimize this? We should take into account that intersection and sum types must exist in their
  //       normal forms, so they will be flattened and each part will be unique. If we sort such types by some
  //       type order, too, we can also use that to walk both arrays once instead of doing nested loops twice.
  //       The normal form could even lead to the result that, if the two types don't have an equal amount of parts,
  //       those parts cannot possibly be equal. Maybe we should try to prove that generally, so that we can be sure
  //       of this property.
  (t1: SumType, t2: SumType) => hasEqualIn(t1.types, t2.types) && hasEqualIn(t2.types, t1.types),
  (t1: IntersectionType, t2: IntersectionType) => hasEqualIn(t1.types, t2.types) && hasEqualIn(t2.types, t1.types),

  (t1: ProductType, t2: ProductType) => {
    const types1 = t1.types
    const types2 = t2.types
    if (!(types1.length === types2.length)) return false
    for (let i = 0; i < types1.length; i += 1) {
      const e1 = types1[i]
      const e2 = types2[i]
      if (!areEqual(e1, e2)) return false
    }
    return true
  },

  (t1: ListType, t2: ListType) => areEqual(t1.element, t2.element),
  (t1: MapType, t2: MapType) => areEqual(t1.key, t2.key) && areEqual(t1.value, t2.value),

  // To check shape type equality, we use the following idea: If all properties in s1 are equal to the properties
  // in s2, and s1 and s2 have the same number of properties, there cannot be a property p2 in s2 that is not already
  // in s1 (by name). Practically, this means that we only have to establish equal length and then compare s1's
  // properties to s2's properties. There's no need to do the reverse.
  (s1: ShapeType, s2: ShapeType) => {
    const s1Keys = Object.keys(s1.propertyTypes)
    if (s1Keys.length !== Object.keys(s2.propertyTypes).length) {
      return false
    }
    for (const p1Name of s1Keys) {
      const p2Type = s2.propertyTypes[p1Name]
      if (!p2Type || !areEqual(s1.propertyTypes[p1Name], p2Type)) return false
    }
    return true
  }
]

/**
 * Checks that the given property types are exactly equal to the property types found in the given struct type.
 */
function arePropertyTypesEqualTo(propertyTypes: PropertyTypes, structType: StructType): boolean {
  for (const p1Name of Object.keys(propertyTypes)) {
    const p2Type = Struct.getPropertyType(structType, p1Name)
    if (!p2Type || !areEqual(propertyTypes[p1Name], p2Type)) {
      return false
    }
  }
  return true
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

// This represents our future, possible hasEqualIn IF we sort the types and find out that sum/intersection types in
// their normal form can by compared piece by piece instead of with these very costly pairwise comparisons.
function hasEqualInOptimized(types1: Array<Type>, types2: Array<Type>): boolean {
  if (types1.length !== types2.length) return false
  for (let i = 0; i < types1.length; i += 1) {
    const t1 = types1[i]
    const t2 = types2[i]
    if (!areEqual(t1, t2)) return false
  }
  return true
}
