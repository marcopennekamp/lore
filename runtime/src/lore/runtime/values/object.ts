import { Type } from '../types/types.ts'
import { LoreValue } from './values.ts'

export interface ObjectValue extends LoreValue {
  object: object
  lore$type: Type // TODO: Needs to be a class type.
}

export const api = {
  create(object: object, type: Type): ObjectValue {
    return { object, lore$type: type }
  }
}
