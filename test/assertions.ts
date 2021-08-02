import { assert, assertEquals, assertExists } from 'https://deno.land/std/testing/asserts.ts'
import { FunctionType, FunctionValue } from '../runtime/src/lore/runtime/functions.ts'
import { ListValue } from '../runtime/src/lore/runtime/lists.ts'
import { MapValue } from '../runtime/src/lore/runtime/maps.ts'
import { ShapeValue } from '../runtime/src/lore/runtime/shapes.ts'
import { StructValue } from '../runtime/src/lore/runtime/structs.ts'
import { SumType } from '../runtime/src/lore/runtime/sums.ts'
import { TupleValue } from '../runtime/src/lore/runtime/tuples.ts'
import { PropertyTypes } from '../runtime/src/lore/runtime/types/declared-types.ts'
import { areEqual } from '../runtime/src/lore/runtime/types/equality.ts'
import { Kind } from '../runtime/src/lore/runtime/types/kinds.ts'
import { Type, Types } from '../runtime/src/lore/runtime/types/types.ts'

export function assertTypeEquals(actual: Type, expected: Type) {
  assert(areEqual(actual, expected), `The type ${Types.stringify(actual)} should be equal to the expected type ${Types.stringify(expected)}.`)
}

export function assertSumTypeParts(actual: Type, partPredicate: (part: Type) => Boolean) {
  assertEquals(actual.kind, Kind.Sum)
  const sumType = <SumType> actual
  sumType.types.forEach((part) => {
    assert(partPredicate(part), `The type ${part} may not be part of the sum type according to the given predicate.`)
  })
}

export function assertIsTuple(actual: TupleValue, elements?: Array<Type>) {
  const actualType = actual.lore$type
  assertEquals(actualType.kind, Kind.Tuple)
  if (elements) {
    const actualElements = actualType.types
    assertEquals(actualElements.length, elements.length)
    actualElements.forEach((actualElement, index) => {
      assertTypeEquals(actualElement, elements[index])
    })
  }
}

export function assertTupleEquals(actual: TupleValue, expected: Array<any>, elementTypes?: Array<Type>) {
  assertIsTuple(actual, elementTypes)
  assertEquals(actual.elements, expected)
}

export function assertIsFunction(actual: FunctionValue<any>, expected?: FunctionType) {
  const actualType = actual.lore$type
  assertEquals(actualType.kind, Kind.Function)
  if (expected) {
    assertTypeEquals(actualType, expected)
  }
}

export function assertIsList(actual: ListValue<any>, element?: Type) {
  const actualType = actual.lore$type
  assertEquals(actualType.kind, Kind.List)
  if (element) {
    assertTypeEquals(actualType.element, element)
  }
}

export function assertListEquals<A>(actual: ListValue<A>, expected: Array<A>, elementType?: Type) {
  assertIsList(actual, elementType)
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

export function assertIsMap(actual: MapValue<any, any>, key?: Type, value?: Type) {
  const actualType = actual.lore$type
  assertEquals(actualType.kind, Kind.Map)
  if (key) {
    assertTypeEquals(actualType.key, key)
  }
  if (value) {
    assertTypeEquals(actualType.value, value)
  }
}

export function assertMapEquals<K, V>(actual: MapValue<K, V>, expected: Array<[K, V]>, keyType?: Type, valueType?: Type) {
  assertIsMap(actual, keyType, valueType)
  assertMapForall(actual, expected, (actual, expected) => assertEquals(actual, expected))
}

export function assertMapForall<K, V, A>(actual: MapValue<K, V>, expected: Array<[K, A]>, assertCondition: (actual: V, expected: A) => void) {
  assertIsMap(actual)

  const actualEntries = getMapEntries(actual)
  expected.forEach(([key, value]) => {
    assert(actualEntries.has(key), `The map must contain an entry with key ${key}.`)
    // @ts-ignore
    assertCondition(actualEntries.get(key), value)
  })
}

/**
 * The MapValue does not contain a store that is a HashMap, because the map is merely read from JSON, which means that
 * `store` lacks the HashMap prototype. We can instead read all the pairs from `_bins`.
 */
function getMapEntries<K, V>(actual: MapValue<K, V>): Map<K, V> {
  return new Map(
    // @ts-ignore
    actual.store._bins
      .filter(entry => !!entry)
      .map(({ key, value }) => [key, value])
  )
}

export function assertIsShape(actual: ShapeValue, properties?: PropertyTypes) {
  assertEquals(actual.lore$type.kind, Kind.Shape)
  if (properties) {
    const actualProperties = actual.lore$type.propertyTypes
    const keys = Object.keys(properties)
    assertEquals(Object.keys(actualProperties).length, keys.length)
    keys.forEach(name => {
      const actualProperty = actualProperties[name]
      const expectedProperty = properties[name]
      assertExists(actualProperty)
      assertTypeEquals(actualProperty, expectedProperty)
    })
  }
}

export function assertShapeEquals(actual: ShapeValue, expected: object, propertyTypes?: PropertyTypes) {
  assertIsShape(actual, propertyTypes)
  assertShapeForall(actual, expected, (actual, expected) => assertEquals(actual, expected))
}

export function assertShapeForall(actual: ShapeValue, expected: object, assertCondition: (actual: any, expected: any) => void) {
  assertIsShape(actual)

  const keys = Object.keys(expected)
  assertEquals(Object.keys(actual).filter(name => name !== 'lore$type').length, keys.length)
  keys.forEach(name => {
    // @ts-ignore
    assertCondition(actual[name], expected[name])
  })
}

export function assertIsStruct(actual: StructValue, fullName: string) {
  const actualType = actual.lore$type
  assertEquals(actualType.kind, Kind.Struct)
  assertEquals(actualType.schema.name, fullName)
}

export function assertStructHasValues(actual: StructValue, expectedFullStructName: string, expectedValues: object) {
  assertIsStruct(actual, expectedFullStructName)
  Object.entries(expectedValues).forEach(kv => {
    const [key, value] = kv
    assertEquals((actual as any)[key], value)
  })
}
