import { TupleType } from './tuples.ts'
import { Kind } from './types/kinds.ts'
import { Type } from './types/types.ts'
import { pairHash } from './utils/hash.ts'
import { Value } from './values.ts'

export interface FunctionType extends Type {
  input: TupleType
  output: Type
}

/**
 * A function value consists of a function type and a Javascript function with parameters fitting the function type.
 */
export interface FunctionValue<R> extends Value {
  callable: (...args: any) => R
  lore$type: FunctionType
}

export const Function = {
  type(input: TupleType, output: Type): FunctionType {
    return { kind: Kind.Function, input, output, hash: pairHash(input, output, 0xf4527105) }
  },

  value<R>(callable: (...args: any) => R, type: FunctionType): FunctionValue<R> {
    return { callable, lore$type: type }
  },

  call<R>(value: FunctionValue<R>, ...args: any): R {
    return value.callable(...args)
  },
}
