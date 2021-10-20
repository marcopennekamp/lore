import { Value } from '../values.ts'
import { Type, Types } from './types.ts'

/**
 * Calculates the Lore type of a value.
 *
 * TODO: If the compiler is sure that a given value is a struct, shape value, etc. with a lore$type attribute,
 *       we should inline the lore$type access to remove the need of calling typeOf. This should bring sizable
 *       performance gains.
 */
export function typeOf(value: any): Type {
  switch (typeof value) {
    case 'number':
      return Types.number
    case 'boolean':
      return Types.boolean
    case 'string':
      return Types.string
    case 'object':
      // TODO: In the case of a Javascript object being given that does not have a type field, we should return some
      //       kind of "dynamic" type. Of course, this first requires us to define a similar notion within Lore
      //       itself.
      if ((<Value> value).lore$type) {
        return (<Value> value).lore$type
      }
      break
  }

  // TODO: Throw a "corresponding Lore type not found" error.
  return Types.any
}
