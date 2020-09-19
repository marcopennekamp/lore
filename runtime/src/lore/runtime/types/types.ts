import { Kind } from './kinds.ts'
import {
  Hashed,
  orderedHashWithSeed,
  pairHash,
  singleHash,
  stringHash,
  stringHashWithSeed,
  unorderedHashWithSeed,
} from '../utils/hash.ts'
import { allExcluding, flattenedUnique } from './util.ts'
import { isSubtype } from './subtyping.ts'

export interface Type extends Hashed {
  kind: Kind
}


export interface AnyType extends Type { }
export const any: AnyType = { kind: Kind.Any, hash: stringHash("any") }

export interface NothingType extends Type { }
export const nothing: NothingType = { kind: Kind.Nothing, hash: stringHash("nothing") }

export interface RealType extends Type { }
export const real: RealType = { kind: Kind.Real, hash: stringHash("real") }

export interface IntType extends Type { }
export const int: IntType = { kind: Kind.Int, hash: stringHash("int") }

export interface BooleanType extends Type { }
export const boolean: BooleanType = { kind: Kind.Boolean, hash: stringHash("boolean") }

export interface StringType extends Type { }
export const string: StringType = { kind: Kind.String, hash: stringHash("string") }


export interface TypeVariable extends Type {
  name: string
  lowerBound: Type
  upperBound: Type
}

export function variable(name: string, lowerBound: Type, upperBound: Type): TypeVariable {
  // TODO: Can we make the hash not only dependent on the variable name?
  return { kind: Kind.TypeVariable, name, lowerBound, upperBound, hash: stringHashWithSeed(name, 0x7ff08f15) }
}


export interface XaryType extends Type {
  types: Array<Type>
}


export interface SumType extends XaryType { }

export function sum(types: Array<Type>): SumType {
  return { kind: Kind.Sum, types, hash: unorderedHashWithSeed(types, 0x85f5fe35) }
}

/**
 * Simplifies the given sum type parts according to our sum type normal form. If the resulting sum type would only
 * contain a single part, we instead return the type itself.
 */
export function sumSimplified(types: Array<Type>): Type {
  // TODO: If we ordered types by some type ordering, we could probably achieve a speedup for subtyping, equality
  //       checking, and also simplification that involves intersection and/or sum types.
  const flattened = flattenedUnique(Kind.Sum, types)

  // Remove strict subtypes of other parts. Conceptually, we have to check for strict subtypes here, but we have
  // already established that no two types in the list are equal. Since the definition of strict subtyping t1 < t2
  // is t1 =/= t2 && t1 <= t2, we can forego the strict check here and just use normal subtyping.
  const simplified = allExcluding(flattened, (self, other) => isSubtype(self, other))

  if (simplified.length === 1) {
    return simplified[0]
  } else {
    return sum(simplified)
  }
}


export interface IntersectionType extends XaryType { }

export function intersection(types: Array<Type>): IntersectionType {
  return { kind: Kind.Intersection, types, hash: unorderedHashWithSeed(types, 0x74a2317d) }
}

export function intersectionSimplified(types: Array<Type>): Type {
  // TODO: If we ordered types by some type ordering, we could probably achieve a speedup for subtyping, equality
  //       checking, and also simplification that involves intersection and/or sum types.
  const flattened = flattenedUnique(Kind.Intersection, types)

  // Remove strict supertypes of other parts. Conceptually, we have to check for strict supertypes here, but we have
  // already established that no two types in the list are equal. Since the definition of strict supertyping t1 > t2
  // is t1 =/= t2 && t1 >= t2, we can forego the strict check here and just use normal subtyping.
  const simplified = allExcluding(flattened, (self, other) => isSubtype(other, self))

  if (simplified.length === 1) {
    return simplified[0]
  } else {
    return intersection(simplified)
  }
}


export interface ProductType extends XaryType { }

export function product(types: Array<Type>): ProductType {
  return { kind: Kind.Product, types, hash: orderedHashWithSeed(types, 0x4baf1ec8) }
}

/**
 * Creates a product type WITHOUT a sensible hash. This should ONLY be used by the compiler to optimize
 * operations that don't require a hash, such as multiple dispatch resolution with a disabled cache.
 */
export function unhashedProduct(types: Array<Type>): ProductType {
  return { kind: Kind.Product, types, hash: 0 }
}

export const unit: ProductType = product([])


export interface ComponentType extends Type {
  underlying: StructType
}

export function component(underlying: StructType): ComponentType {
  if (underlying.kind !== Kind.Class) {
    throw Error("A component type must have an underlying class type.")
  }
  return { kind: Kind.Component, underlying, hash: singleHash(underlying, 0x4cab1ec0) }
}


export interface ListType extends Type {
  element: Type
}

export function list(element: Type): ListType {
  return { kind: Kind.List, element, hash: singleHash(element, 0xfb04146c) }
}


export interface MapType extends Type {
  key: Type
  value: Type
}

export function map(key: Type, value: Type): MapType {
  return { kind: Kind.Map, key, value, hash: pairHash(key, value, 0xbeda0294) }
}


export interface StructType extends Type {
  name: string
  declaredSupertypes: Array<TraitType>
  componentTypes: Array<ComponentType>
  ownedBy?: Type
  isEntity: Boolean
  // members
}

export function structType(
  name: string, declaredSupertypes: Array<TraitType>, componentTypes: Array<ComponentType>, ownedBy: Type | undefined, isEntity: boolean,
): StructType {
  return { kind: Kind.Class, name, declaredSupertypes, componentTypes, ownedBy, isEntity, hash: stringHashWithSeed(name, 0x38ba128e) }
}


export interface TraitType extends Type {
  name: string
  // supertraits
  // componentTypes
  // ownedBy
  // isEntity
}
