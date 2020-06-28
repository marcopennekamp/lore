import { Type } from './types.ts'
import { any, boolean, int, real, string } from './types.ts'
import { LoreValue } from '../values/values.ts'

/**
 * Calculates the Lore type of a value.
 */
export function typeOf(value: any): Type {
  switch (typeof value) {
    case 'number':
      if (Number.isInteger(value)) {
        return int
      } else {
        return real
      }
    case 'boolean':
      return boolean
    case 'string':
      return string
    case 'object':
      // TODO: In the case of a Javascript object being given that does not have a type field, we should return some
      //       kind of "dynamic" type. Of course, this first requires us to define a similar notion within Lore
      //       itself.
      if (value.hasOwnProperty('lore$type')) {
        return (<LoreValue> value).lore$type
      }
      break
  }

  // TODO: Throw a "corresponding Lore type not found" error.
  return any
}
