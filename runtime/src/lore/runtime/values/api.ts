import { api as list } from './list.ts'
import { api as object } from './object.ts'
import { api as tuple } from './tuple.ts'
import { api as map } from './map.ts'
import { numberHash, stringHash } from '../utils/hash.ts'

export default {
  // Value APIs.
  tuple,
  list,
  map,
  object,

  // Core functions operating on values of any type.
  areEqual: (a: any, b: any) => a == b,
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
