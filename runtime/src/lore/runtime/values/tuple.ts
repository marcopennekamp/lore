import { ProductType, unit } from '../types/types.ts'
import { LoreValue } from './values.ts'

export interface TupleValue extends LoreValue {
  array: Array<any>
  lore$type: ProductType
}

export const api = {
  create(array: Array<any>, type: ProductType): TupleValue {
    return { array, lore$type: type }
  },

  unit(): TupleValue {
    return { array: [], lore$type: unit }
  },

  get(tuple: TupleValue, index: number): any {
    // TODO: Surely proper bounds checking, no?
    return tuple.array[index];
  },
}
