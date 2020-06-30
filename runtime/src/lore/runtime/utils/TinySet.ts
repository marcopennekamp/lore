/**
 * A "tiny set" is a Set-like structure that is implemented using an array, which makes it faster to use with
 * very few elements.
 */
export type TinySet<A> = Array<A>

export const TinySet = {
  has<A>(array: TinySet<A>, value: A): boolean {
    for (let i = 0; i < array.length; i += 1) {
      if (array[i] === value) return true
    }
    return false
  },
  add<A>(array: TinySet<A>, value: A): void {
    if (TinySet.has(array, value)) return
    array.push(value)
  }
}
