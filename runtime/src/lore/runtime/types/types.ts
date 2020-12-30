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
import { LazyValue } from '../utils/LazyValue.ts'

// TODO: Create a toString function for types.
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

  // TODO: Combine shape types.

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


export interface DeclaredTypeSchema {
  name: string
  supertraits: Array<TraitType>
}

export interface DeclaredType extends Type {
  schema: DeclaredTypeSchema

  /**
   * Whether this declared type is the one that represents the compile-time type, i.e. its open property types are
   * exactly equal to the compile-time property types.
   */
  isArchetype: boolean
}


export type PropertyTypes = { [key: string]: Type }
export type LazyPropertyTypes = { [key: string]: LazyValue<Type> }

export interface StructSchema extends DeclaredTypeSchema {
  /**
   * The struct's properties, with each name as the key, and their respective compile-time types.
   *
   * Since open property types are handled by the struct's transpiled instantiation function, we do not need to
   * label properties as open at run-time. The specific type instance's property types will simply override the
   * property types found in the schema's map.
   *
   * The property type map must contain lazy types, because schemas are created at the start of the program with no
   * respect to property type order. Property types can easily reference declared types, which would lead to undefined
   * types if types are loaded in the wrong order. Since schema property types will be infrequently accessed, likely
   * only for structural typing, making the type lazy is acceptable.
   */
  propertyTypes: LazyPropertyTypes
}

export function structSchema(
  name: string,
  supertraits: Array<TraitType>,
  propertyTypes: LazyPropertyTypes,
): StructSchema {
  return { name, supertraits, propertyTypes }
}

/**
 * The type of a struct. For each struct that is instantiated (assuming the struct has open properties), a new struct
 * type is created, as the list of open property types differs based on the actual properties present at run-time.
 */
export interface StructType extends DeclaredType {
  schema: StructSchema

  // TODO: Once traits can extend shape types, this needs to also apply to traits.
  /**
   * The actual run-time types of the struct's properties IF they deviate from their compile-time type. The map may be
   * empty or undefined.
   */
  propertyTypes?: PropertyTypes
}

// TODO: Interning struct types might bring big performance gains for multiple dispatch, because the more open properties a
//       struct has, the more expensive the areEqual test gets. This test HAS to be performed at least once per
//       dispatch cache access. Interned struct types would allow us to decide struct type equality via referential
//       equality as there would be exactly one struct type per property type combination. Many structs may only ever
//       have their archetype, too, which would further speed up type checking.

export function struct(schema: StructSchema, isArchetype: boolean, propertyTypes?: PropertyTypes): StructType {
  return {
    kind: Kind.Struct,
    schema,
    isArchetype,
    propertyTypes,
    // TODO: The hash of the struct type must not only depend on the name, but also on the open property types. Otherwise,
    //       structs with different property types altogether get the same hashes, which is bad for dispatch cache
    //       performance.
    hash: stringHashWithSeed(schema.name, 0x38ba128e),
  }
}


export interface TraitSchema extends DeclaredTypeSchema { }

export function traitSchema(name: string, supertraits: Array<TraitType>): TraitSchema {
  return { name, supertraits }
}

/**
 * A trait type. Only one type is instantiated for each trait, as their supertypes are not dependent on run-time
 * values. Thus, a trait type is also always an archetype.
 */
export interface TraitType extends DeclaredType {
  schema: TraitSchema
}

export function trait(schema: TraitSchema): TraitType {
  return { kind: Kind.Trait, schema, isArchetype: true, hash: stringHashWithSeed(schema.name, 0x38ba128e) }
}
