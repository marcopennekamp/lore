import { ListType } from '../types/types.ts'
import { LoreValue } from './values.ts'

/**
 * An immutable list, the standard list type of Lore.
 *
 * Right now, the list is backed by an array which is copied on every list manipulation. This will result in
 * very slow operations especially for large lists. The plan is to replace this implementation with Scala's
 * Vector implementation, which are basically immutable arrays with excellent properties across the board.
 * We are not using singly-linked lists for this naive implementation because we do not want to encourage
 * prepending as a standard way of building a list. Rather, appending should be the way to go in general,
 * as it is more intuitive to build a list from left to right, in the way it is iterated.
 */
export interface ListValue<A> extends LoreValue {
  array: Array<A>
  lore$type: ListType
}

export const api = {
  create<A>(array: Array<A>, type: ListType): ListValue<A> {
    return { array, lore$type: type }
  },

  /**
   * Creates a new (immutable) list by appending the element to the given list. The type has to be supplied manually
   * as we don't calculate it at run-time. Rather, the compiler decides what kind of type this list receives. This
   * behavior is in line with other collections and also fits with how type arguments will work for declared types: at
   * the point of construction, the collection's or struct's/trait's type is decided based on its type at compile-time.
   */
  append<A, B extends A>(list: ListValue<A>, element: B, type: ListType): ListValue<A> {
    return { array: [...list.array, element], lore$type: type }
  },

  get<A>(list: ListValue<A>, index: number): A {
    // TODO: Bounds checking?
    return list.array[index];
  },

  forEach<A, R>(list: ListValue<A>, f: (e: A) => R): void {
    const array = list.array
    for (let i = 0; i < array.length; i += 1) {
      f(array[i])
    }
  },

  length<A>(list: ListValue<A>): number {
    return list.array.length;
  },
}
