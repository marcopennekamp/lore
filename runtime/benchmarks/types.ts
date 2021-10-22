import { Intersection } from '../src/lore/runtime/intersections.ts'
import { Lists } from '../src/lore/runtime/lists.ts'
import { Map } from '../src/lore/runtime/maps.ts'
import { Shape } from '../src/lore/runtime/shapes.ts'
import { Sum } from '../src/lore/runtime/sums.ts'
import { Tuple } from '../src/lore/runtime/tuples.ts'
import { areEqual } from '../src/lore/runtime/types/equality.ts'
import { isSubtype } from '../src/lore/runtime/types/subtyping.ts'
import { Types } from '../src/lore/runtime/types/types.ts'
import { benchmark } from './benchmark.ts'

const sum1 = Sum.type([Sum.type([Types.string, Types.int]), Types.boolean])
const sum2 = Sum.type([Types.string, Types.int, Types.boolean])
const sum3 = Sum.type([Types.string, Types.int, Types.boolean])
const intersection1 = Intersection.type([Types.string, Types.int, Types.boolean])
const intersection2 = Intersection.type([Types.boolean, Types.string, Types.int])
const intersection3 = Intersection.type([Types.string, Types.int, Types.boolean])
const list1 = Lists.type(Types.int)
const list2 = Lists.type(Types.int)
const list3 = Lists.type(Map.type(Types.string, Types.int))
const list4 = Lists.type(Map.type(Types.string, Types.int))
const tuple1 = Tuple.type([sum2, intersection1, list3])
const tuple2 = Tuple.type([sum2, intersection1, list3])
const tuple3 = Tuple.type([Sum.type([Types.string, Types.int, Types.boolean]), Intersection.type([Types.string, Types.int, Types.boolean]), Lists.type(Map.type(Types.string, Types.int))])
const tuple4 = Tuple.type([Sum.type([Types.string, Types.int, Types.boolean]), Intersection.type([Types.string, Types.int, Types.boolean]), Lists.type(Map.type(Types.string, Types.int))])
const shape1 = Shape.type({ x: Types.real, y: Types.real })
const shape2 = Shape.type({ x: Types.real, y: Types.real, c: Types.int, d: Types.string })
const shape3 = Shape.type({ d: Types.string, c: Types.int, x: Types.real, y: Types.real })

export function benchmarkAreEqual(): void {
  console.log('Benchmarking areEqual:')

  const times = 10000000
  benchmark('nested sum', () => areEqual(sum1, sum2), times)
  benchmark('equal sum (reference)', () => areEqual(sum2, sum2), times)
  benchmark('equal sum (structural)', () => areEqual(sum2, sum3), times)
  benchmark('basic types', () => areEqual(Types.int, Types.string), times)
  benchmark('equal intersection (structural)', () => areEqual(intersection1, intersection2), times)
  benchmark('equal intersection (structural, favorable order)', () => areEqual(intersection1, intersection3), times)
  benchmark('equal basic type list (structural)', () => areEqual(list1, list2), times)
  benchmark('equal map list (structural)', () => areEqual(list3, list4), times)
  benchmark('not equal list', () => areEqual(list1, list3), times)
  // The tuple tests represent a real-world scenario of calling a function with a complex input type.
  // The crux of the matter is that at least one call of areEqual will be necessary even with the dispatch cache
  // to verify equality, since a map can have duplicate entries for a given hash key.
  benchmark('equal tuple (structural, shallow)', () => areEqual(tuple1, tuple2), times)
  benchmark('equal tuple (structural, deep)', () => areEqual(tuple3, tuple4), times)
  benchmark('equal shape (structural)', () => areEqual(shape2, shape3), times)
  benchmark('empty run', () => { }, times) // This can be used to gauge the benchmark's own overhead.
  console.log()
}

export function benchmarkIsSubtype(): void {
  console.log('Benchmarking isSubtype:')

  const times = 10000000
  benchmark('nested sum', () => isSubtype(sum1, sum2), times)
  benchmark('subtyped sum (reference)', () => isSubtype(sum2, sum2), times)
  benchmark('subtyped sum (structural)', () => isSubtype(sum2, sum3), times)
  benchmark('not subtyped basic types', () => isSubtype(Types.int, Types.string), times)
  benchmark('subtyped intersection (structural)', () => isSubtype(intersection1, intersection2), times)
  benchmark('subtyped intersection (structural, favorable order)', () => isSubtype(intersection1, intersection3), times)
  benchmark('subtyped basic type list (structural)', () => isSubtype(list1, list2), times)
  benchmark('subtyped map list (structural)', () => isSubtype(list3, list4), times)
  benchmark('not subtyped list', () => isSubtype(list1, list3), times)
  benchmark('subtyped tuple (structural, shallow)', () => isSubtype(tuple1, tuple2), times)
  benchmark('subtyped tuple (structural, deep)', () => isSubtype(tuple3, tuple4), times)
  benchmark('subtyped shape (structural)', () => isSubtype(shape2, shape1), times)
  benchmark('empty run', () => { }, times) // This can be used to gauge the benchmark's own overhead.
  console.log()
}
