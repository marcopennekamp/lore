import { stringHash } from '../utils/hash.ts'
import { Kind } from './kinds.ts'
import { Type } from './types.ts'

export interface AnyType extends Type { }
export interface NothingType extends Type { }
export interface NumberType extends Type { }
export interface BooleanType extends Type { }
export interface StringType extends Type { }

export const BasicType = {
  any: { kind: Kind.Any, hash: stringHash("any") } as AnyType,
  nothing: { kind: Kind.Nothing, hash: stringHash("nothing") } as NothingType,
  number: { kind: Kind.Number, hash: stringHash("number") } as NumberType,
  boolean: { kind: Kind.Boolean, hash: stringHash("boolean") } as BooleanType,
  string: { kind: Kind.String, hash: stringHash("string") } as StringType,
}
