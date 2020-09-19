import { StructType } from '../types/types.ts'
import { LoreValue } from './values.ts'

/**
 * An object consists of the lore$type property and all of its members directly.
 */
export interface ObjectValue extends LoreValue {
  lore$type: StructType
}

export const api = {
  /**
   * Creates a new Lore object with the given members and struct type. The 'members' object will be used as the final
   * object value, so it should be seen as permanently borrowed.
   */
  create(members: object, type: StructType): ObjectValue {
    const value = members as ObjectValue
    value.lore$type = type
    return value
  }
}
