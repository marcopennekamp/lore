import { stringHash } from '../utils/hash.ts'
import { Kind } from './kinds.ts'
import { Type } from './types.ts'

export interface AnyType extends Type { }
export interface NothingType extends Type { }
export interface RealType extends Type { }
export interface IntType extends Type { }
export interface BooleanType extends Type { }
export interface StringType extends Type { }

export const BasicType = {
  any: { kind: Kind.Any, hash: stringHash("any") } as AnyType,
  nothing: { kind: Kind.Nothing, hash: stringHash("nothing") } as NothingType,
  real: { kind: Kind.Real, hash: stringHash("real") } as RealType,
  int: { kind: Kind.Int, hash: stringHash("int") } as IntType,
  boolean: { kind: Kind.Boolean, hash: stringHash("boolean") } as BooleanType,
  string: { kind: Kind.String, hash: stringHash("string") } as StringType,
}
