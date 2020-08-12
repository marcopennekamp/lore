import { product, ProductType, unit } from '../types/types.ts'
import { LoreValue } from './values.ts'
import { typeOf } from '../types/typeof.ts'

export interface TupleValue extends LoreValue {
  elements: Array<any>
  lore$type: ProductType
}

// TODO: Consider exporting the API with some more descriptive name. As it stands it's hard to import the API in, say, maps.ts.
export const api = {
  create(elements: Array<any>): TupleValue {
    const elementTypes = new Array(elements.length)
    for (let i = 0; i < elements.length; i += 1) {
      elementTypes[i] = typeOf(elements[i])
    }
    return { elements, lore$type: product(elementTypes) }
  },

  unit(): TupleValue {
    return { elements: [], lore$type: unit }
  },

  get(tuple: TupleValue, index: number): any {
    // TODO: Surely proper bounds checking, no?
    return tuple.elements[index];
  },
}
