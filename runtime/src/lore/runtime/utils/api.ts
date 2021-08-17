import { stringify } from '../types/stringify.ts'
import { TinyMap } from './TinyMap.ts'
import { TinySet } from './TinySet.ts'
import { HashMap } from './HashMap.ts'
import { EqualsFunction, HashFunction } from './definitions.ts'
import { Type } from '../types/types.ts'
import { LazyValue } from './LazyValue.ts'
import { TypeMaps } from './TypeMap.ts'

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
  typeMap: TypeMaps,
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
    ambiguousCall(functionName: string, inputType: Type) {
      throw new Error(`The multi-function ${functionName} is ambiguous for the input type ${stringify(inputType)}.`);
    },
    emptyFit(functionName: string, inputType: Type) {
      throw new Error(`Could not find an implementation of ${functionName} for the input type ${stringify(inputType)}.`);
    },
    missingImplementation(functionName: string, parameterType: string, argumentType: Type) {
      throw new Error(`The abstract function ${functionName}${parameterType} is missing an implementation for ${stringify(argumentType)}.`);
    },
  },
}
