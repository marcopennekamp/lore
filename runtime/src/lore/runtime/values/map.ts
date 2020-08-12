import { MapType, product } from '../types/types.ts'
import { LoreValue } from './values.ts'
import { HashMap } from '../utils/HashMap.ts'
import { EqualsFunction, HashFunction } from '../utils/definitions.ts'
import { TupleValue, api as tupleApi } from './tuple.ts'
import { typeOf } from '../types/typeof.ts'

export interface MapValue<K, V> extends LoreValue {
  store: HashMap<K, V>
  lore$type: MapType
}

export const api = {
  /**
   * Creates a map from the initial array of entries with the given hash and equals functions.
   */
  create<K, V>(entries: Array<[K, V]>, type: MapType, hash: HashFunction<K>, equals: EqualsFunction<K>): MapValue<K, V> {
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
      const entryTuple = tupleApi.create([entry.key, entry.value]);
      yield entryTuple
      result = iterator.next()
    }
  },

  // TODO: Rename to size?
  length<K, V>(map: MapValue<K, V>): number {
    return map.store.size()
  },
}
