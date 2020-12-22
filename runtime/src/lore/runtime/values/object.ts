import { StructType } from '../types/types.ts'
import { LoreValue } from './values.ts'

/**
 * An object consists of the lore$type property and all of its properties directly. Components are contained in their
 * own object to make retrieval possible. Without this tiered structure, we'd have to tag each component member
 * individually as a component.
 *
 * TODO: Rename to StructValue?
 */
export interface ObjectValue extends LoreValue {
  // TODO: Rethink the lore$ naming scheme to bring it more in line with other transpiled names.
  lore$type: StructType
}

export const api = {
  /**
   * Creates a new Lore object with the given properties, components and struct type. The 'properties' object will be
   * used as the final object value, so it should be seen as permanently borrowed.
   */
  create(properties: any, type: StructType): ObjectValue {
    const value = properties as ObjectValue
    value.lore$type = type
    return value
  },
}
