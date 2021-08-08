import { areEqual } from './equality.ts'
import { Kind } from './kinds.ts'
import { Type, XaryType } from './types.ts'

/**
 * Flattens an array of types of the given kind and ensures that each element of the resulting type list is
 * unique in respect to areEqual.
 *
 * This function flattens only at a single level of depth. Deeper flattening should not be necessary if
 * flattening is applied diligently during the construction of new types.
 */
export function flattenedUnique(kind: Kind, types: Array<Type>): Array<Type> {
  const flattened: Array<Type> = []
  for (let i = 0; i < types.length; i += 1) {
    const part = types[i]
    if (part.kind === kind) {
      const xaryPart = <XaryType> part
      for (let j = 0; j < xaryPart.types.length; j += 1) {
        addUnique(xaryPart.types[j], flattened)
      }
    } else {
      addUnique(part, flattened)
    }
  }
  return flattened
}

/**
 * Ensures that each element of the resulting type list is unique in respect to areEqual.
 */
export function unique(types: Array<Type>): Array<Type> {
  const result: Array<Type> = []
  for (const type of types) {
    addUnique(type, result)
  }
  return result
}

function addUnique(candidate: Type, types: Array<Type>) {
  for (const type of types) {
    // Don't add a candidate that is already part of the type list. We have to use areEqual for correctness here, but
    // most comparisons should be caught either by the reference check (signifying equality) or by the hash and kind
    // checks (signifying inequality).
    if (areEqual(candidate, type)) return
  }
  types.push(candidate)
}

/**
 * Filters the given array, returning a new array only with elements that have been included. Whether an
 * element is excluded depends on the predicate, which is applied to the element in question and all other
 * elements. If the predicate returns true for at least one element, that element is excluded from the
 * result.
 */
export function allExcluding<A>(elements: Array<A>, predicate: (self: A, other: A) => Boolean): Array<A> {
  // TODO: Can we optimize this in the context of types?
  const length = elements.length
  const result: Array<A> = []
  for (let i = 0; i < length; i += 1) {
    const self = elements[i]
    let j = 0
    for (; j < length; j += 1) {
      const other = elements[j]
      if (i != j && predicate(self, other)) break
    }
    if (j == length) {
      result.push(self)
    }
  }
  return result
}
