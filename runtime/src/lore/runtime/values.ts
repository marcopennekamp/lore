import { Type } from './types/types.ts'
import { numberHash, stringHash } from './utils/hash.ts'

export interface Value {
  lore$type: Type
}

/**
 * This API contains core functions operating on values of any type.
 */
export const Values = {
  areEqual: (a: any, b: any) => a === b,
  isLessThan: (a: any, b: any) => a < b,
  hash: (value: any) => {
    switch (typeof value) {
      case 'boolean': return stringHash(value ? 'true' : 'false')
      case 'number': return numberHash(value, 0x2a391e16d)
      case 'string': return stringHash(value)
      case 'object': throw new Error('Objects don\'t have a default implementation of hash. Please provide a custom implementation.')
    }
  },
  toString: (value: any) => value.toString(),
}
