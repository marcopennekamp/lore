import { areEqual } from '../types/equality.ts'
import { Type } from '../types/types.ts'
import { HashMap } from './HashMap.ts'

export interface TypeMap<K extends Type, V> extends HashMap<K, V> {
}

export const TypeMaps = {
  create<K extends Type, V>(): TypeMap<K, V> {
    return new HashMap<K, V>(
      (type) => type.hash,
      areEqual,
    )
  },
}
