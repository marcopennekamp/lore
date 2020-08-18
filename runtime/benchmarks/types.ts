import { boolean, int, intersection, list, map, product, string, sum } from '../src/lore/runtime/types/types.ts'
import { areEqual, areEqualIterative } from '../src/lore/runtime/types/equality.ts'
import { benchmark } from './benchmark.ts'
import { isSubtype } from '../src/lore/runtime/types/subtyping.ts'

const sum1 = sum([sum([string, int]), boolean])
const sum2 = sum([string, int, boolean])
const sum3 = sum([string, int, boolean])
const int1 = intersection([string, int, boolean])
const int2 = intersection([boolean, string, int])
const int3 = intersection([string, int, boolean])
const list1 = list(int)
const list2 = list(int)
const list3 = list(map(string, int))
const list4 = list(map(string, int))
const product1 = product([sum2, int1, list3])
const product2 = product([sum2, int1, list3])
const product3 = product([sum([string, int, boolean]), intersection([string, int, boolean]), list(map(string, int))])
const product4 = product([sum([string, int, boolean]), intersection([string, int, boolean]), list(map(string, int))])

export function benchmarkAreEqual(): void {
  console.log('Benchmarking areEqual:')

  const times = 10000000
  benchmark('nested sum', () => areEqual(sum1, sum2), times)
  benchmark('equal sum (reference)', () => areEqual(sum2, sum2), times)
  benchmark('equal sum (structural)', () => areEqual(sum2, sum3), times)
  benchmark('basic types', () => areEqual(int, string), times)
  benchmark('equal intersection (structural)', () => areEqual(int1, int2), times)
  benchmark('equal intersection (structural, favorable order)', () => areEqual(int1, int3), times)
  benchmark('equal basic type list (structural)', () => areEqual(list1, list2), times)
  benchmark('equal map list (structural)', () => areEqual(list3, list4), times)
  benchmark('not equal list', () => areEqual(list1, list3), times)
  // The product tests represent a real-world scenario of calling a function with a complex input type.
  // The crux of the matter is that at least one call of areEqual will be necessary even with the dispatch cache
  // to verify equality, since a map can have duplicate entries for a given hash key.
  benchmark('equal product (structural, shallow)', () => areEqual(product1, product2), times)
  benchmark('equal product (structural, deep)', () => areEqual(product3, product4), times)
  benchmark('empty run', () => { }, times) // This can be used to gauge the benchmark's own overhead.
  console.log()
}

export function benchmarkAreEqualIterative(): void {
  console.log('Benchmarking areEqualIterative:')

  const times = 10000000
  benchmark('nested sum', () => areEqualIterative(sum1, sum2), times)
  benchmark('equal sum (reference)', () => areEqualIterative(sum2, sum2), times)
  benchmark('equal sum (structural)', () => areEqualIterative(sum2, sum3), times)
  benchmark('basic types', () => areEqualIterative(int, string), times)
  benchmark('equal intersection (structural)', () => areEqualIterative(int1, int2), times)
  benchmark('equal intersection (structural, favorable order)', () => areEqualIterative(int1, int3), times)
  benchmark('equal basic type list (structural)', () => areEqualIterative(list1, list2), times)
  benchmark('equal map list (structural)', () => areEqualIterative(list3, list4), times)
  benchmark('not equal list', () => areEqualIterative(list1, list3), times)
  // The product tests represent a real-world scenario of calling a function with a complex input type.
  // The crux of the matter is that at least one call of areEqualIterative will be necessary even with the dispatch cache
  // to verify equality, since a map can have duplicate entries for a given hash key.
  benchmark('equal product (structural, shallow)', () => areEqualIterative(product1, product2), times)
  benchmark('equal product (structural, deep)', () => areEqualIterative(product3, product4), times)
  benchmark('empty run', () => { }, times) // This can be used to gauge the benchmark's own overhead.
  console.log()
}

export function benchmarkIsSubtype(): void {
  console.log('Benchmarking isSubtype:')

  const times = 10000000
  benchmark('nested sum', () => isSubtype(sum1, sum2), times)
  benchmark('subtyped sum (reference)', () => isSubtype(sum2, sum2), times)
  benchmark('subtyped sum (structural)', () => isSubtype(sum2, sum3), times)
  benchmark('not subtyped basic types', () => isSubtype(int, string), times)
  benchmark('subtyped intersection (structural)', () => isSubtype(int1, int2), times)
  benchmark('subtyped intersection (structural, favorable order)', () => isSubtype(int1, int3), times)
  benchmark('subtyped basic type list (structural)', () => isSubtype(list1, list2), times)
  benchmark('subtyped map list (structural)', () => isSubtype(list3, list4), times)
  benchmark('not subtyped list', () => isSubtype(list1, list3), times)
  benchmark('subtyped product (structural, shallow)', () => isSubtype(product1, product2), times)
  benchmark('subtyped product (structural, deep)', () => isSubtype(product3, product4), times)
  benchmark('empty run', () => { }, times) // This can be used to gauge the benchmark's own overhead.
  console.log()
}
