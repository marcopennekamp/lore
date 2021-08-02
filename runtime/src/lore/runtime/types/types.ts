import { TraitType } from '../traits.ts'
import { finalize, Hashed, mix, mixLast, stringHash, stringHashWithSeed } from '../utils/hash.ts'
import { LazyValue } from '../utils/LazyValue.ts'
import { areEqual } from './equality.ts'
import { fits, fitsMonomorphic, fitsPolymorphic } from './fit.ts'
import { Introspection } from './introspection.ts'
import { Kind } from './kinds.ts'
import { isPolymorphic, variables } from './polymorphy.ts'
import { stringify } from './stringify.ts'
import { isSubtype } from './subtyping.ts'
import { TypeVariable, Variance } from './type-variables.ts'
import { typeOf } from './typeof.ts'

// TODO: Create a toString function for types.
export interface Type extends Hashed {
  kind: Kind
}

export interface XaryType extends Type {
  types: Array<Type>
}


export interface AnyType extends Type { }
const any: AnyType = { kind: Kind.Any, hash: stringHash("any") }

export interface NothingType extends Type { }
const nothing: NothingType = { kind: Kind.Nothing, hash: stringHash("nothing") }

export interface RealType extends Type { }
const real: RealType = { kind: Kind.Real, hash: stringHash("real") }

export interface IntType extends Type { }
const int: IntType = { kind: Kind.Int, hash: stringHash("int") }

export interface BooleanType extends Type { }
const boolean: BooleanType = { kind: Kind.Boolean, hash: stringHash("boolean") }

export interface StringType extends Type { }
const string: StringType = { kind: Kind.String, hash: stringHash("string") }


export type PropertyTypes = { [key: string]: Type }
export type LazyPropertyTypes = { [key: string]: LazyValue<Type> }

export interface DeclaredTypeSchema {
  name: string
  supertraits: Array<TraitType>
}

export interface DeclaredType extends Type {
  schema: DeclaredTypeSchema

  /**
   * Whether this declared type is the one that represents the compile-time type, i.e. its open property types and type
   * variables are exactly equal to the compile-time property types.
   */
  isArchetype: boolean
}

/**
 * This has the same implementation as `unorderedHashWithSeed` mainly for performance reasons.
 */
export function hashPropertyTypes(propertyTypes: PropertyTypes, seed: number): number {
  let a = 0, b = 0, i = 0, c = 1
  for (const name of Object.keys(propertyTypes)) {
    let h = 0x4cb3052e
    h = mix(h, stringHash(name))
    h = mixLast(h, propertyTypes[name].hash)
    h = finalize(h, 1)
    a += h
    b ^= h
    if (h != 0) c *= h
  }

  let h = seed
  h = mix(h, a)
  h = mix(h, b)
  h = mixLast(h, c)
  return finalize(h, i)
}


export const Types = {
  // Basic types.
  any,
  nothing,
  real,
  int,
  boolean,
  string,

  // Type variables.
  variable(name: string, index: number, lowerBound: Type, upperBound: Type, variance: Variance): TypeVariable {
    return { kind: Kind.TypeVariable, name, index, lowerBound, upperBound, variance, hash: stringHashWithSeed(name, 0x7ff08f15) }
  },
  variance: Variance,

  // Generally applicable type operations.
  isSubtype,
  areEqual,
  fits,
  fitsMonomorphic,
  fitsPolymorphic,
  typeOf,
  isPolymorphic,
  variables,
  stringify,

  // The type introspection API.
  introspection: Introspection,
}
