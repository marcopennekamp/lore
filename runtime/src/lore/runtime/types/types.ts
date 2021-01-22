import { TraitType } from '../traits.ts'
import { Hashed, stringHash, stringHashWithSeed } from '../utils/hash.ts'
import { LazyValue } from '../utils/LazyValue.ts'
import { areEqual } from './equality.ts'
import { fits, fitsMonomorphic, fitsPolymorphic } from './fit.ts'
import { Introspection } from './introspection.ts'
import { Kind } from './kinds.ts'
import { isPolymorphic, variables } from './polymorphy.ts'
import { isSubtype } from './subtyping.ts'
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


export interface TypeVariable extends Type {
  name: string
  lowerBound: Type
  upperBound: Type
}


export type PropertyTypes = { [key: string]: Type }
export type LazyPropertyTypes = { [key: string]: LazyValue<Type> }

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


export const Types = {
  // Basic types.
  any,
  nothing,
  real,
  int,
  boolean,
  string,

  // Type variables.
  variable(name: string, lowerBound: Type, upperBound: Type): TypeVariable {
    // TODO: Can we make the hash not only dependent on the variable name?
    return { kind: Kind.TypeVariable, name, lowerBound, upperBound, hash: stringHashWithSeed(name, 0x7ff08f15) }
  },

  // Generally applicable type operations.
  isSubtype,
  areEqual,
  fits,
  fitsMonomorphic,
  fitsPolymorphic,
  typeOf,
  isPolymorphic,
  variables,

  // The type introspection API.
  introspection: Introspection,
}
