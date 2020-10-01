import { TinyMap } from './TinyMap.ts'
import { TinySet } from './TinySet.ts'
import { HashMap } from './HashMap.ts'
import { EqualsFunction, HashFunction } from './definitions.ts'
import { Type } from '../types/types.ts'
import { areEqual } from '../types/equality.ts'
import { LazyValue } from './LazyValue.ts'

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
    create<K extends Type, V>(): HashMap<K, V> {
      return new HashMap<K, V>(
        (type) => type.hash,
        areEqual,
      )
    },
  },
  lazy: {
    /**
     * Creates a lazy value from the given generator function.
     */
    of<V>(generate: () => V): LazyValue<V> {
      return new LazyValue(generate)
    }
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
