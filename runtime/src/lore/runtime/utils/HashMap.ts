import { MapEntry } from './MapEntry.ts'
import { EqualsFunction, HashFunction } from './definitions.ts'

/**
 * Configurable hash map with user-supplied hash and equality functions. Uses open addressing and linear probing
 * to resolve collision.
 *
 * Based on https://github.com/thi-ng/umbrella/tree/master/packages/associative. Thanks to Karsten Schmidt!
 */
export class HashMap<K, V> implements Iterable<MapEntry<K, V>> {

  /**
   * The starting bins length of a fresh hash map.
   */
  static readonly STARTING_CAP = 16

  /**
   * The load factor of all hash maps. If the hash map exceeds this relative load, it will be resized.
   */
  static readonly LOAD_FACTOR = 0.75

  /**
   * A custom hash function used to hash the keys.
   */
  readonly hash: HashFunction<K>

  /**
   * A custom equals function used to compare the keys.
   */
  readonly equals: EqualsFunction<K>

  /**
   * A bitmask used to cudgel hashes down to the right size.
   */
  private _mask!: number

  /**
   * The current size of the hash map.
   */
  private _size!: number

  /**
   * The maximum size of the hash map when considering load factor.
   */
  private _maxSize!: number

  /**
   * The individual entries of the hash map.
   */
  private _bins!: Array<MapEntry<K, V>>

  constructor(hash: HashFunction<K>, equals: EqualsFunction<K>) {
    this.hash = hash
    this.equals = equals
    this.init(HashMap.STARTING_CAP)
  }

  private init(cap: number): void {
    this._mask = cap - 1
    this._size = 0
    this._maxSize = cap * HashMap.LOAD_FACTOR
    this._bins = new Array(cap)
  }

  [Symbol.iterator]() {
    return this.entries()
  }

  /**
   * Creates an iterator that goes through all entries of the map.
   */
  *entries(): IterableIterator<MapEntry<K, V>> {
    const bins = this._bins
    for (let i = 0; i < bins.length; i += 1) {
      const entry = bins[i]
      if (entry) {
        yield entry
      }
    }
  }

  /**
   * The key & value args given the callback `fn` MUST be treated as
   * readonly/immutable. This could be enforced via TS, but would
   * break ES6 Map interface contract.
   *
   * @param f
   */
  forEach(f: (v: V, k: K) => void): void {
    const bins = this._bins
    for (let i = 0; i < bins.length; i += 1) {
      const entry = bins[i]
      if (entry) {
        f(entry.value, entry.key)
      }
    }
  }

  /**
   * Clears all entries from the map.
   */
  clear(): void {
    this.init(HashMap.STARTING_CAP)
  }

  /**
   * Whether this map has the given key.
   */
  has(key: K): boolean {
    const i = this.find(key)
    return i >= 0 && this._bins[i] !== undefined
  }

  /**
   * Retrieves the value with the given key or returns undefined.
   */
  get(key: K): V | undefined {
    return this.getOrElseInternal(key, undefined)
  }

  /**
   * Retrieves the value with the given key or returns the fallback value.
   */
  getOrElse(key: K, fallback: V): V {
    return <V> this.getOrElseInternal(key, fallback)
  }

  private getOrElseInternal(key: K, fallback?: V): V | undefined {
    const i = this.find(key)
    if (i < 0) return fallback
    const bin = this._bins[i]
    return bin ? bin.value : fallback
  }

  /**
   * The current number of entries stored in the hash map.
   */
  size(): number {
    return this._size
  }

  /**
   * Sets the given key to the given value.
   */
  set(key: K, value: V): void {
    let i = this.find(key)
    if (i >= 0) {
      const bin = this._bins[i]
      if (bin) {
        bin.value = value
        return
      }
    }

    if (this._size > this._maxSize) {
      this.resize()
      i = this.find(key)
    }

    this._bins[i] = { key, value }
    this._size += 1
  }

  /**
   * Deletes the entry identified by the given key from the map.
   */
  delete(key: K): boolean {
    let i = this.find(key)
    const bins = this._bins
    if (i >= 0 && !bins[i]) {
      return false
    }

    // Delete the entry at the given index and shift all other entries back.
    // TODO: Can we achieve this without a while(true) loop?
    this._size -= 1
    const m = this._mask
    let j = i
    let k: number
    while (true) {
      delete bins[i]
      do {
        j = (j + 1) & m
        if (!bins[j]) return true
        k = this.hash(bins[j].key) & m
      } while (i <= j ? i < k && k <= j : i < k || k <= j)
      bins[i] = bins[j]
      i = j
    }
  }

  /**
   * Finds the entry with the given key or returns -1.
   */
  protected find(key: K): number {
    const m = this._mask
    const bins = this._bins
    const equals = this.equals
    let binsToCheck = m // Technically this is binsToCheck - 1 to save an addition.
    let h = this.hash(key) & m
    let bin = bins[h]
    while (bin && !equals(bin.key, key)) {
      binsToCheck -= 1
      if (binsToCheck < 0) return -1
      h = (h + 1) & m
      bin = bins[h]
    }
    return h
  }

  /**
   * Doubles the size of this map to accommodate more entries.
   */
  protected resize(): void {
    const src = this._bins
    const cap = (this._mask + 1) * 2
    this.init(cap)
    for (let i = 0; i < src.length; i += 1) {
      const entry = src[i]
      if (entry) {
        // TODO: We can save an object allocation here if we duplicate the code of set into a second
        //       function upsert that takes an already existing entry. We would have to duplicate code
        //       so that we don't have to create a new entry to call set in case it degrades performance
        //       when values are updated frequently.
        this.set(entry.key, entry.value)
      }
    }
  }

}
