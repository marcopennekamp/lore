/**
 * A "tiny map" is a Map-like structure that is implemented using an array, because it is faster with
 * very few elements.
 */
export type TinyMap<A, B> = Array<TinyMapEntry<A, B>>

/**
 * A tiny map entry is simply a cheap object. This is its interface.
 */
export interface TinyMapEntry<A, B> {
  key: A
  value: B
}

export const TinyMap = {
  get<A, B>(array: TinyMap<A, B>, key: A): B | undefined {
    for (let i = 0; i < array.length; i += 1) {
      const entry = array[i]
      if (entry.key === key) {
        return entry.value
      }
    }
    return undefined
  },
  set<A, B>(array: TinyMap<A, B>, key: A, value: B): void {
    // Find the key and overwrite the value.
    for (let i = 0; i < array.length; i += 1) {
      const entry = array[i]
      if (entry.key === key) {
        entry.value = value
        return
      }
    }

    // Key could not be found!
    array.push({ key, value })
  }
}
