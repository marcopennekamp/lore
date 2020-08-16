import { boolean, int, intersection, list, map, string, sum } from '../src/lore/runtime/types/types.ts'
import { areEqual } from '../src/lore/runtime/types/equality.ts'
import { benchmark } from './benchmark.ts'

export function benchmarkAreEqual(): void {
  console.log('Benchmarking areEqual:')
  const sum1 = sum([sum([string, int]), boolean])
  const sum2 = sum([string, int, boolean])
  const sum3 = sum([string, int, boolean])
  const int1 = intersection([string, int, boolean])
  const int2 = intersection([boolean, string, int])
  const int3 = intersection([string, int, boolean])
  const list1 = list(int)
  const list2 = list(string)
  const list3 = list(map(string, int))
  const list4 = list(map(string, int))

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
  benchmark('empty run', () => { }, times) // This can be used to gauge the benchmark's own overhead.
}
