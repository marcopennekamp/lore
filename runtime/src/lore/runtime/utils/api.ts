import { TinyMap } from './TinyMap.ts'
import { TinySet } from './TinySet.ts'
import { HashMap } from './HashMap.ts'
import { EqualsFunction, HashFunction } from './definitions.ts'
import { Type } from '../types/types.ts'
import { areEqual } from '../types/equality.ts'

export default {
  tinyMap: {
    get: TinyMap.get,
    add: TinyMap.set,
  },
  tinySet: {
    has: TinySet.has,
    add: TinySet.add,
  },
  hashMap: {
    create<K, V>(hash: HashFunction<K>, equals: EqualsFunction<K>) {
      return new HashMap<K, V>(hash, equals)
    },
  },
  typeMap: {
    create<V>() {
      return new HashMap<Type, V>(
        (type) => type.hash, // TODO: We could inline this and change the signature of the map implementation.
        areEqual,
      )
    },
  },
}
