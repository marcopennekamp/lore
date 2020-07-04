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
    create<K, V>(hash: HashFunction<K>, equals: EqualsFunction<K>): HashMap<K, V> {
      return new HashMap<K, V>(hash, equals)
    },
  },
  typeMap: {
    create<V>(): HashMap<Type, V> {
      return new HashMap<Type, V>(
        (type) => type.hash, // TODO: We could inline this and change the signature of the map implementation.
        areEqual,
      )
    },
  },
  // TODO: I feel like this should rather be part of some "core" or "error" API.
  error: {
    ambiguousCall(functionName: string, inputType: Type | string) {
      throw new Error(`The multi-function ${functionName} is ambiguous for the input type ${inputType}.`);
    },
    emptyFit(functionName: string, inputType: Type | string) {
      throw new Error(`Could not find an implementation of ${functionName} for the input type ${inputType}.`);
    },
    missingImplementation(functionName: string, parameterType: Type | string, argumentType: Type | string) {
      throw new Error(`The abstract function ${functionName}${parameterType} is missing an implementation for ${argumentType}.`);

    },
  },
}
