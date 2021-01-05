import { Tuple, TupleValue } from './tuples.ts'
import { Kind } from './types/kinds.ts'
import { Type } from './types/types.ts'
import { EqualsFunction, HashFunction } from './utils/definitions.ts'
import { pairHash } from './utils/hash.ts'
import { HashMap } from './utils/HashMap.ts'
import { Value } from './values.ts'

export interface MapType extends Type {
  key: Type
  value: Type
}

export interface MapValue<K, V> extends Value {
  store: HashMap<K, V>
  lore$type: MapType
}

export const Map = {
  type(key: Type, value: Type): MapType {
    return { kind: Kind.Map, key, value, hash: pairHash(key, value, 0xbeda0294) }
  },

  /**
   * Creates a map from the initial array of entries with the given hash and equals functions.
   */
  value<K, V>(entries: Array<[K, V]>, type: MapType, hash: HashFunction<K>, equals: EqualsFunction<K>): MapValue<K, V> {
    const store = new HashMap<K, V>(hash, equals)
    for (let i = 0; i < entries.length; i += 1) {
      const entry = entries[i]
      store.set(entry[0], entry[1])
    }
    return { store, lore$type: type }
  },

  set<K1, V1, K2 extends K1, V2 extends V1>(map: MapValue<K1, V1>, key: K2, value: V2): void {
    map.store.set(key, value)
  },

  get<K, V>(map: MapValue<K, V>, key: K): V {
    const value = map.store.get(key)
    if (value === undefined) {
      // TODO: This should return an Option value! It's not very useful to just error out.
      throw new Error(`The key ${key} does not exist in the given map.`)
    }
    return value
  },

  contains<K, V>(map: MapValue<K, V>, key: K): boolean {
    return map.store.has(key)
  },

  *entries<K, V, R>(map: MapValue<K, V>): IterableIterator<TupleValue> {
    const iterator = map.store.entries()
    let result = iterator.next()
    while (!result.done) {
      const entry = result.value
      const entryTuple = Tuple.value([entry.key, entry.value]);
      yield entryTuple
      result = iterator.next()
    }
  },

  // TODO: Rename to size?
  length<K, V>(map: MapValue<K, V>): number {
    return map.store.size()
  },
}
