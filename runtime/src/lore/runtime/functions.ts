import { Kind } from './types/kinds.ts'
import { Type } from './types/types.ts'
import { pairHash, singleHash } from './utils/hash.ts'

export interface FunctionType extends Type {
  input: Type
  output: Type
}

export const Function = {
  type(input: Type, output: Type): FunctionType {
    return { kind: Kind.Function, input, output, hash: pairHash(input, output, 0xf4527105) }
  },
}
