import { Hashed, stringHashWithSeed } from '../utils/hash.ts'
import { BasicType } from './basic-types.ts'
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

export const Types = {
  // Basic types.
  any: BasicType.any,
  nothing: BasicType.nothing,
  real: BasicType.real,
  int: BasicType.int,
  boolean: BasicType.boolean,
  string: BasicType.string,

  // Type variables.
  variable(index: number, lowerBound: Type, upperBound: Type, variance: Variance, fullName: string): TypeVariable {
    return { kind: Kind.TypeVariable, fullName, index, lowerBound, upperBound, variance, hash: stringHashWithSeed(fullName, 0x7ff08f15) }
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
