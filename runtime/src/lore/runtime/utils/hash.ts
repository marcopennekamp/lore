/**
 * We've used the following MurmurHash3 implementations as a basis (especially the first):
 *    - https://github.com/scala/scala/blob/2.13.x/src/library/scala/util/hashing/MurmurHash3.scala
 *    - https://github.com/garycourt/murmurhash-js
 */

/**
 * An object with a hash, to be used with collection hashing.
 */
export interface Hashed {
  hash: number
}

/**
 * Creates a hash from the given string using a predefined seed.
 */
export function stringHash(string: string): number {
  return stringHashWithSeed(string, 0xf7ca7fd2)
}

/**
 * Creates a hash from the given string.
 */
export function stringHashWithSeed(string: string, seed: number): number {
  // We split the string into sections of four UTF-16 characters (taking the lower 8 bits only) and
  // then consider the remainder separately.
  const length = string.length
  const remainderLength = length & 3 // string.length % 4
  const sectionsLength = length - remainderLength
  let hash = seed
  let i = 0
  while (i < sectionsLength) {
    const data =
      (( string.charCodeAt(i) & 0xff)) |
      (( string.charCodeAt(++i) & 0xff) << 8) |
      (( string.charCodeAt(++i) & 0xff) << 16) |
      (( string.charCodeAt(++i) & 0xff) << 24)
    ++i
    hash = mix(hash, data)
  }

  let k = 0
  switch (remainderLength) {
    case 3:
      k ^= (string.charCodeAt(i + 2) & 0xff) << 16
    case 2:
      k ^= (string.charCodeAt(i + 1) & 0xff) << 8
    case 1:
      k ^= (string.charCodeAt(i) & 0xff)
      hash = mixLast(hash, k)
  }

  return finalize(hash, length)
}

/**
 * Creates an ordered hash from the given array of hashed elements using a predefined seed.
 */
export function orderedHash<A extends Hashed>(array: Array<A>): number {
  return orderedHashWithSeed(array, 0xb592f7ae)
}

/**
 * Creates an ordered hash from the given array of hashed elements. You should use different kinds of seeds for
 * different kinds of collection types so that empty arrays of different kinds are not generating the same hash.
 */
export function orderedHashWithSeed<A extends Hashed>(array: Array<A>, seed: number): number {
  let i = 0
  let h = seed
  for (; i < array.length; i += 1) {
    h = mix(h, array[i].hash)
  }
  return finalize(h, i)
}

/**
 * Creates an unordered hash from the given array of hashed elements using a predefined seed.
 */
export function unorderedHash<A extends Hashed>(array: Array<A>): number {
  return unorderedHashWithSeed(array, 0xe73a8b15)
}

/**
 * Creates an unordered hash from the given array of hashed elements. You should use different kinds of seeds for
 * different kinds of collection types so that empty arrays of different kinds are not generating the same hash.
 *
 * We compute a hash that is symmetric in its argument, so that the order of elements doesn't matter. This is
 * useful for hashing sets.
 */
export function unorderedHashWithSeed<A extends Hashed>(array: Array<A>, seed: number): number {
  // TODO: Does this work well for large arrays?
  //       See also: https://crypto.stackexchange.com/questions/54544/how-to-to-calculate-the-hash-of-an-unordered-set
  let a = 0, b = 0, i = 0, c = 1
  for (; i < array.length; i += 1) {
    const h = array[i].hash
    a += h
    b ^= h
    if (h != 0) c *= h
  }

  let h = seed
  h = mix(h, a)
  h = mix(h, b)
  h = mixLast(h, c)
  return finalize(h, i)
}

export function numberHash(value: number, seed: number): number {
  let h = seed
  h = mixLast(h, value)
  return finalize(h, 1)
}

/**
 * Hashes a single hashed value. If your values can belong to several different "kinds" all distributed in the same
 * space, such as different classes with the same underlying attribute e1, you can use the seed to differentiate
 * these kinds of values.
 */
export function singleHash<A extends Hashed>(e1: A, seed: number): number {
  return numberHash(e1.hash, seed)
}

/**
 * Hashes two hashed values. If your values can belong to several different "kinds" all distributed in the same
 * space, such as different classes with the same underlying attribute e1 and e2, you can use the seed to
 * differentiate these kinds of values.
 */
export function pairHash<A extends Hashed, B extends Hashed>(e1: A, e2: B, seed: number): number {
  return pairHashRaw(e1.hash, e2.hash, seed)
}

export function pairHashRaw(e1: number, e2: number, seed: number): number {
  let h = seed
  h = mix(h, e1)
  h = mixLast(h, e2)
  return finalize(h, 1)
}

/**
 * Mix in a block of data into an intermediate hash value.
 */
export function mix(hash: number, data: number): number {
  let h = mixLast(hash, data)
  h = (h << 13) | (h >>> 19)
  let hb = ((((h & 0xffff) * 5) + ((((h >>> 16) * 5) & 0xffff) << 16))) & 0xffffffff
  return ((hb & 0xffff) + 0x6b64) + ((((hb >>> 16) + 0xe654) & 0xffff) << 16)
}

/**
 * May optionally be used as the last mixing step. Is a little bit faster than mix,
 * as it does no further mixing of the resulting hash. For the last element this is not
 * necessary as the hash is thoroughly mixed during finalization anyway.
 */
export function mixLast(hash: number, data: number): number {
  let k = data
  k = ((((k & 0xffff) * 0xcc9e2d51) + ((((k >>> 16) * 0xcc9e2d51) & 0xffff) << 16))) & 0xffffffff
  k = (k << 15) | (k >>> 17)
  k = ((((k & 0xffff) * 0x1b873593) + ((((k >>> 16) * 0x1b873593) & 0xffff) << 16))) & 0xffffffff
  return hash ^ k
}

/**
 * Finalize a hash to incorporate the length and make sure all bits avalanche.
 */
export function finalize(hash: number, length: number): number {
  let h = hash ^ length

  h ^= h >>> 16
  h = (((h & 0xffff) * 0x85ebca6b) + ((((h >>> 16) * 0x85ebca6b) & 0xffff) << 16)) & 0xffffffff
  h ^= h >>> 13
  h = ((((h & 0xffff) * 0xc2b2ae35) + ((((h >>> 16) * 0xc2b2ae35) & 0xffff) << 16))) & 0xffffffff
  h ^= h >>> 16

  return h >>> 0
}
