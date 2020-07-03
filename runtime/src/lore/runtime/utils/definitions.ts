export type HashFunction<K> = (k: K) => number
export type EqualsFunction<K> = (k1: K, k2: K) => boolean
