import { finalize, mix, mixLast, stringHash } from '../utils/hash.ts'
import { LazyValue } from '../utils/LazyValue.ts'
import { Type } from './types.ts'

export type PropertyTypes = { [key: string]: Type }
export type LazyPropertyTypes = { [key: string]: LazyValue<Type> }

/**
 * This has the same implementation as `unorderedHashWithSeed` mainly for performance reasons.
 */
export function hashPropertyTypes(propertyTypes: PropertyTypes, seed: number): number {
  let a = 0, b = 0, i = 0, c = 1
  for (const name of Object.keys(propertyTypes)) {
    let h = 0x4cb3052e
    h = mix(h, stringHash(name))
    h = mixLast(h, propertyTypes[name].hash)
    h = finalize(h, 1)
    a += h
    b ^= h
    if (h != 0) c *= h
  }

  let h = seed
  h = mix(h, a)
  h = mix(h, b)
  h = mixLast(h, c)
  return finalize(h, i)
}
