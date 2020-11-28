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
import { isSubtype, Subtyping } from './subtyping.ts'
import { HashMap } from '../utils/HashMap.ts'
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


// TODO: We could easily intern component types by giving each declared type exactly one ComponentType instance...

export interface ComponentType extends Type {
  underlying: DeclaredType
}

export function component(underlying: DeclaredType): ComponentType {
  if (underlying.kind !== Kind.Struct && underlying.kind !== Kind.Trait) {
    throw Error('A component type must have an underlying declared type.')
  }
  if (!underlying.schema.isOwnable) {
    throw Error('A component type must contain an ownable declared type.')
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


export interface DeclaredTypeSchema {
  name: string
  supertraits: Array<TraitType>

  /**
   * The owned-by type of the declared type. It must be passed as a lazy value, because it needs to be loaded after all
   * other types have been initialised. The reason is simple: we can have two types that mention each other in their
   * owned-by types, and so if we evaluated this eagerly, one of the types would have a null reference!
   */
  ownedBy: LazyValue<Type>

  isOwnable: boolean
  isEntity: boolean
}

export interface DeclaredType extends Type {
  schema: DeclaredTypeSchema
  componentTypes: Array<ComponentType>

  /**
   * Whether this declared type is the one that represents the compile-time type, i.e. its component types are
   * exactly equal to the compile-time component types.
   */
  isArchetype: boolean
}


export interface MemberDefinition {
  name: string
  // TODO: type?
}

export interface StructSchema extends DeclaredTypeSchema {
  properties: Array<MemberDefinition>
  components: Array<MemberDefinition>

  /**
   * A cache that maps a declared type, used by the compiler to access a given component, to this struct's corresponding
   * component name. The cache significantly increases access performance, because without it we will have to resort to
   * checking subtypes. This cache only has to exist once per schema, as the actual run-time component types have no
   * bearing on field names.
   *
   * The cache is instantiated during the first retrieval.
   */
  componentAccessCache?: HashMap<DeclaredType, string>
}

export function structSchema(
  name: string,
  supertraits: Array<TraitType>,
  ownedBy: LazyValue<Type>,
  isOwnable: boolean,
  isEntity: boolean,
  properties: Array<MemberDefinition>,
  components: Array<MemberDefinition>,
): StructSchema {
  return { name, supertraits, ownedBy, isOwnable, isEntity, properties, components }
}

/**
 * The type of a struct. For each struct that is instantiated, a new struct type is created, as the list of component
 * types differs based on the actual components present at run-time.
 */
export interface StructType extends DeclaredType {
  schema: StructSchema
}

// TODO: Interning struct types might bring big performance gains for multiple dispatch, because the more components a
//       struct has, the more expensive the areEqual test gets. This test HAS to be performed at least once per
//       dispatch cache access. Interned struct types would allow us to decide struct type equality via referential
//       equality as there would be exactly one struct type per component type combination. Many structs may only ever
//       have their archetype, too, which would further speed up type checking.

export function struct(schema: StructSchema, componentTypes: Array<ComponentType>, isArchetype: boolean): StructType {
  const type: StructType = {
    kind: Kind.Struct,
    schema,
    componentTypes,
    isArchetype,
    // TODO: The hash of the struct type must not only depend on the name, but also on the component types. Otherwise,
    //       structs with different component types altogether get the same hashes, which is bad for dispatch cache
    //       performance.
    hash: stringHashWithSeed(schema.name, 0x38ba128e),
  }

  // Check that all component types can be owned by the given struct.
  for (let i = 0; i < componentTypes.length; i += 1) {
    const componentType = componentTypes[i]
    const ownedBy = componentType.underlying.schema.ownedBy.value()
    if (!Subtyping.NoOwnedBy.isSubtype(type, ownedBy)) {
      const componentTypeNames = componentTypes.map(c => c.underlying.schema.name)
      throw Error(`The struct type ${schema.name} with the components ${componentTypeNames.join(', ')} cannot own the component ${componentType.underlying.schema.name}, which has an owned-by type of ${ownedBy}.`)
    }
  }

  return type
}


export interface TraitSchema extends DeclaredTypeSchema { }

export function traitSchema(
  name: string, supertraits: Array<TraitType>, ownedBy: LazyValue<Type>, isOwnable: boolean, isEntity: boolean,
): TraitSchema {
  return { name, supertraits, ownedBy, isOwnable, isEntity }
}

/**
 * A trait type. Only one type is instantiated for each trait, as their supertypes are not dependent on run-time
 * values. Thus, a trait type is also always an archetype. Component types could be kept in the TraitSchema,
 * but to achieve symmetry with structs, they are kept in the TraitType.
 *
 * The component types of a trait correspond to the "inheritedComponentTypes" of the compiler. That is, we only
 * include component types that are not subsumed by other component types, keeping a minimal version of the list.
 */
export interface TraitType extends DeclaredType {
  schema: TraitSchema
}

export function trait(schema: TraitSchema, componentTypes: Array<ComponentType>): TraitType {
  return { kind: Kind.Trait, schema, componentTypes, isArchetype: true, hash: stringHashWithSeed(schema.name, 0x38ba128e) }
}
