import { Value } from '../values.ts'
import { Type, Types } from './types.ts'

/**
 * Calculates the Lore type of a value.
 */
export function typeOf(value: any): Type {
  switch (typeof value) {
    case 'number':
      if (Number.isInteger(value)) {
        return Types.int
      } else {
        return Types.real
      }
    case 'boolean':
      return Types.boolean
    case 'string':
      return Types.string
    case 'object':
      // TODO: In the case of a Javascript object being given that does not have a type field, we should return some
      //       kind of "dynamic" type. Of course, this first requires us to define a similar notion within Lore
      //       itself.
      if (value.hasOwnProperty('lore$type')) {
        return (<Value> value).lore$type
      }
      break
  }

  // TODO: Throw a "corresponding Lore type not found" error.
  return Types.any
}
