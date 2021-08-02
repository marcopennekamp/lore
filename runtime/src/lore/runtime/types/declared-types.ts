import { TraitType } from '../traits.ts'
import { finalize, mix, mixLast, stringHash } from '../utils/hash.ts'
import { LazyValue } from '../utils/LazyValue.ts'
import { Type } from './types.ts'

export type PropertyTypes = { [key: string]: Type }
export type LazyPropertyTypes = { [key: string]: LazyValue<Type> }

// TODO (schemas): Rename to DeclaredSchema.
export interface DeclaredTypeSchema {
  name: string
  //parameters: Array<TypeVariable>

  /**
   * A list of directly extended traits, potentially containing uninstantiated type parameters.
   */
  supertraits: Array<TraitType>
}

export interface DeclaredType extends Type {
  schema: DeclaredTypeSchema



  /**
   * Whether this declared type is the one that represents the compile-time type, i.e. its open property types and type
   * variables are exactly equal to the compile-time property types.
   */
  isArchetype: boolean
}

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
