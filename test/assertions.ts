import { assert, assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../runtime/src/lore/runtime/lists.ts'
import { StructValue } from '../runtime/src/lore/runtime/structs.ts'
import { SumType } from '../runtime/src/lore/runtime/sums.ts'
import { TupleValue } from '../runtime/src/lore/runtime/tuples.ts'
import { Kind } from '../runtime/src/lore/runtime/types/kinds.ts'
import { Type } from '../runtime/src/lore/runtime/types/types.ts'

export function assertIsTuple(actual: TupleValue, elementCount: Number) {
  assertEquals(actual.lore$type.kind, Kind.Product)
  assertEquals(actual.lore$type.types.length, elementCount)
}

export function assertTupleEquals(actual: TupleValue, expected: Array<any>) {
  assertIsTuple(actual, expected.length)
  assertEquals(actual.elements, expected)
}

export function assertIsStruct(actual: StructValue, fullName: string) {
  assertEquals(actual.lore$type.kind, Kind.Struct)
  assertEquals(actual.lore$type.schema.name, fullName)
}

export function assertStructHasValues(actual: StructValue, expectedFullStructName: string, expectedValues: object) {
  assertIsStruct(actual, expectedFullStructName)
  Object.entries(expectedValues).forEach(kv => {
    const [key, value] = kv
    assertEquals((actual as any)[key], value)
  })
}

export function assertIsList(actual: ListValue<any>) {
  assertEquals(actual.lore$type.kind, Kind.List)
}

export function assertListEquals<A>(actual: ListValue<A>, expected: Array<A>) {
  assertIsList(actual)
  assertEquals(actual.array, expected)
}

export function assertListEqualsUnordered(actual: ListValue<any>, expected: Array<any>) {
  assertIsList(actual)
  expected.forEach(x => {
    assert(actual.array.includes(x), `The actual list does not contain an expected value ${x}.`)
  })
  actual.array.forEach(x => {
    assert(expected.includes(x), `The actual list contains an unexpected value ${x}.`)
  })
}

export function assertListForall<A, B>(actual: ListValue<A>, expected: Array<B>, assertCondition: (actual: A, expected: B) => void) {
  assertIsList(actual)
  assertEquals(
    actual.array.length,
    expected.length,
    `The lists of actual (${actual.array}) and expected (${expected}) values must be of equal length.`
  )

  actual.array.forEach((act, index) => {
    const exp = expected[index]
    assertCondition(act, exp)
  })
}

export function assertSumTypeParts(actual: Type, partPredicate: (part: Type) => Boolean) {
  assertEquals(actual.kind, Kind.Sum)
  const sumType = <SumType> actual
  sumType.types.forEach((part) => {
    assert(partPredicate(part), `The type ${part} may not be part of the sum type according to the given predicate.`)
  })
}
