import { ListType } from '../types/types.ts'
import { LoreValue } from './values.ts'

// TODO: Using arrays here sucks if we want an immutable list, like the Scala list. We should rather implement
//       our own singly-linked list.

export interface ListValue<A> extends LoreValue {
  array: Array<A>
  lore$type: ListType
}

export const api = {
  create<A>(array: Array<A>, type: ListType): ListValue<A> {
    return { array, lore$type: type }
  },

  append<A, B extends A>(list: ListValue<A>, element: B): ListValue<A> {
    // TODO: Don't operate on the list itself but return a new list.
    list.array.push(element)
    return list
  },

  forEach<A, R>(list: ListValue<A>, f: (e: A) => R): void {
    const array = list.array
    for (let i = 0; i < array.length; i += 1) {
      f(array[i])
    }
  }
}
