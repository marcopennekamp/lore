import type { List } from 'https://deno.land/x/immutable@4.0.0-rc.14-deno/node_modules/immutable/dist/immutable-nonambient.d.ts'
import { List as ListConstructor } from 'https://deno.land/x/immutable@4.0.0-rc.14-deno/node_modules/immutable/dist/immutable.es.js'
import { FunctionValue } from './functions.ts'
import { Sum } from './sums.ts'
import { Kind } from './types/kinds.ts'
import { Type } from './types/types.ts'
import { singleHash } from './utils/hash.ts'
import { Value } from './values.ts'

export interface ListType extends Type {
  element: Type
}

/**
 * An immutable list, the standard list type of Lore. This is backed by an immutable-js list.
 */
export interface ListValue<A> extends Value {
  elements: List<A>
  lore$type: ListType
}

export const Lists = {
  type(element: Type): ListType {
    return { kind: Kind.List, element, hash: singleHash(element, 0xfb04146c) }
  },

  value<A>(array: Array<A>, type: ListType): ListValue<A> {
    return Lists.fromImmutableJs(ListConstructor(array), type)
  },

  // TODO (lists): Use `empty` instead of `value` when creating empty lists to avoid an empty array allocation.
  empty(type: ListType): ListValue<any> {
    return Lists.fromImmutableJs(ListConstructor(undefined), type)
  },

  fromImmutableJs<A>(elements: List<A>, type: ListType): ListValue<A> {
    return { elements: elements, lore$type: type }
  },

  /**
   * Creates a new list by appending the element to the given list. The type has to be supplied manually as we don't
   * always calculate it at run-time.
   */
  append<A, B extends A>(list: ListValue<A>, element: B, type: ListType): ListValue<A> {
    return Lists.fromImmutableJs(list.elements.push(element), type)
  },

  /**
   * Creates a new list by appending the element to the given list. The new list is assigned the type from the old
   * list, as this function is only invoked when the compiler is sure that the appends operation doesn't change the
   * type of the list.
   */
  appendUntyped<A, B extends A>(list: ListValue<A>, element: B): ListValue<A> {
    return Lists.fromImmutableJs(list.elements.push(element), list.lore$type)
  },

  get<A>(list: ListValue<A>, index: number): A | undefined {
    // TODO (lists): We should secure this with checking for `undefined`. The Pyramid implementation should return an
    //               Option (`get`), an alternative (`get` with alternative), or throw an error (`get!`).
    return list.elements.get(index)
  },

  length<A>(list: ListValue<A>): number {
    return list.elements.size
  },

  concat<A, B>(as: ListValue<A>, bs: ListValue<B>): ListValue<A | B> {
    const result = as.elements.concat(bs.elements)
    const resultType = Lists.type(Sum.simplified([as.lore$type.element, bs.lore$type.element]))
    return Lists.fromImmutableJs(result, resultType)
  },

  slice<A>(list: ListValue<A>, startIndex: number, length: number): ListValue<A> {
    return Lists.fromImmutableJs(list.elements.slice(startIndex, startIndex + length), list.lore$type)
  },

  flatten<A>(list: ListValue<ListValue<A>>): ListValue<A> {
    const result = list.elements.flatMap(list2 => list2.elements)
    return Lists.fromImmutableJs(result, <ListType> list.lore$type.element)
  },

  map<A, B>(list: ListValue<A>, f: FunctionValue<B>): ListValue<B> {
    const result = list.elements.map(f.callable)
    const resultType = Lists.type(f.lore$type.output)
    return Lists.fromImmutableJs(result, resultType)
  },

  flatMap<A, B>(list: ListValue<A>, f: FunctionValue<ListValue<B>>): ListValue<B> {
    const result = list.elements.flatMap(v => f.callable(v).elements)
    const resultType = Lists.type((<ListType> f.lore$type.output).element)
    return Lists.fromImmutableJs(result, resultType)
  },

  filter<A>(list: ListValue<A>, predicate: FunctionValue<boolean>): ListValue<A> {
    const result = list.elements.filter(predicate.callable)
    return Lists.fromImmutableJs(result, list.lore$type)
  },
}
