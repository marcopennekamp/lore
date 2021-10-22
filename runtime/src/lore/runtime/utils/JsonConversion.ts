import { Lists } from '../lists.ts'
import { ShapeType } from '../shapes.ts'
import { StructType } from '../structs.ts'
import { TupleValue } from '../tuples.ts'
import { Kind } from '../types/kinds.ts'
import { Type } from '../types/types.ts'

export const JsonConversion = {
  /**
   * Converts a Lore value to JSON.
   */
  toJson(value: any): string {
    return JSON.stringify(value, stringifyReplacer)
  },

  /**
   * Converts a JSON string to a Lore value, using `toLore`.
   */
  toLoreFromString(json: string): any {
    return JsonConversion.toLore(JSON.parse(json))
  },

  /**
   * Converts a JSON value to a Lore value, possibly modifying `value` in-place.
   *
   * This function handles the following value kinds specially:
   *  - `ListValue`s are backed by immutable-js, and thus their `elements` representation in JSON is a plain Javascript
   *    array. We have to convert the `elements` to an immutable-js List before the `ListValue` can be used.
   *
   * All other lore values must of course be processed as well so that nested lore values can be handled correctly.
   */
  toLore(value: any): any {
    if (!value?.lore$type) {
      return value
    }

    const type = <Type> value.lore$type
    switch (type.kind) {
      case Kind.Struct:
        toLoreHandleProperties(value, (<StructType> type).schema.propertyOrder)
        break

      case Kind.Tuple:
        const tuple = <TupleValue> value
        tuple.elements = tuple.elements.map(v => JsonConversion.toLore(v))
        break

      case Kind.List:
        const elements = (<Array<any>> value.elements).map(v => JsonConversion.toLore(v))
        return Lists.value(elements, value.lore$type)

      // TODO (maps): This code is horrible, but will be replaced soon anyway, when we use immutable-js HAMTs as the
      //              map backend.
      case Kind.Map:
        // TODO (maps): Implement this for the HAMT immutable-js map backend.
        break

      case Kind.Shape:
        const propertyNames = Object.keys((<ShapeType> type).propertyTypes)
        toLoreHandleProperties(value, propertyNames)
        break
    }

    return value
  }
}

/**
 * Filters properties from JSON.stringify that would lead to circular references.
 */
function stringifyReplacer(key: string, value: any) {
  if (key === 'representative') return undefined
  return value
}

function toLoreHandleProperties(value: any, propertyNames: Array<string>): void {
  for (const propertyName of propertyNames) {
    value[propertyName] = JsonConversion.toLore(value[propertyName])
  }
}
