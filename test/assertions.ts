import { assertEquals, assert } from 'https://deno.land/std/testing/asserts.ts'
import { ObjectValue } from '../runtime/src/lore/runtime/values/object.ts'
import { Kind } from '../runtime/src/lore/runtime/types/kinds.ts'
import { ListValue } from '../runtime/src/lore/runtime/values/list.ts'
import { TupleValue } from '../runtime/src/lore/runtime/values/tuple.ts'

export function assertIsTuple(actual: TupleValue) {
  assertEquals(actual.lore$type.kind, Kind.Product)
}

export function assertTupleEquals(actual: TupleValue, expected: Array<any>) {
  assertIsTuple(actual)
  assertEquals(actual.elements, expected)
}

export function assertIsStruct(actual: ObjectValue, fullName: string) {
  assertEquals(actual.lore$type.kind, Kind.Struct)
  assertEquals(actual.lore$type.schema.name, fullName)
}

export function assertStructHasValues(actual: ObjectValue, expectedFullStructName: string, expectedValues: object) {
  assertIsStruct(actual, expectedFullStructName)
  Object.entries(expectedValues).forEach(kv => {
    const [key, value] = kv
    assertEquals((actual as any)[key], value)
  })
}

export function assertIsList(actual: ListValue<any>) {
  assertEquals(actual.lore$type.kind, Kind.List)
}

export function assertListEquals(actual: ListValue<any>, expected: Array<any>) {
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
