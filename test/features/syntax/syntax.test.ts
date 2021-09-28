import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { Function } from '../../../runtime/src/lore/runtime/functions.ts'
import { List, ListValue } from '../../../runtime/src/lore/runtime/lists.ts'
import { Map, MapValue } from '../../../runtime/src/lore/runtime/maps.ts'
import { Shape, ShapeValue } from '../../../runtime/src/lore/runtime/shapes.ts'
import { Sum } from '../../../runtime/src/lore/runtime/sums.ts'
import { Tuple, TupleValue } from '../../../runtime/src/lore/runtime/tuples.ts'
import { Types } from '../../../runtime/src/lore/runtime/types/types.ts'
import {
  assertIsFunction, assertIsList, assertIsMap, assertIsShape, assertListEquals, assertListForall, assertMapEquals,
  assertMapForall, assertShapeEquals, assertShapeForall, assertTupleEquals,
} from '../../assertions.ts'
import { LoreTest } from '../../base.ts'

const base = 'features/syntax'

Deno.test(`${base}/append`, async () => {
  const result: ListValue<any> = await LoreTest.run(`${base}/append.lore`)
  assertIsList(result)

  const lists = result.array
  assertListEquals(lists[0], [10], Types.int)
  assertListEquals(lists[1], [5.5], Types.real)
  assertListEquals(lists[2], ['hello'], Types.string)
  assertListEquals(lists[3], [12, 14.5, 10], Types.real)
  assertListEquals(lists[4], [12, 14.5, 5.5], Types.real)
  assertListEquals(lists[5], [12, 14.5, 'hello'], Sum.type([Types.real, Types.string]))
  assertListEquals(lists[6], [44, -5, 7, 10], Types.int)
  assertListEquals(lists[7], [44, -5, 7, 5.5], Types.real)
  assertListEquals(lists[8], [44, -5, 7, 'hello'], Sum.type([Types.int, Types.string]))
  assertListEquals(lists[9], [44, -5, 7, 'hello', 'world'], Sum.type([Types.int, Types.string]))
})

Deno.test(`${base}/associativity`, async () => {
  const result: number = await LoreTest.run(`${base}/associativity.lore`)
  assertEquals(result, 18)
})

Deno.test(`${base}/call-line-stretching`, async () => {
  const result: TupleValue = await LoreTest.run(`${base}/call-line-stretching.lore`)
  assertEquals(result.lore$type, Tuple.unitType)
})

Deno.test(`${base}/conditionals`, async () => {
  const result: ListValue<number | boolean> = await LoreTest.run(`${base}/conditionals.lore`)
  assertListEquals(result, [
    false, false, true,
    21, 25, 25, 24, 29,
    1, 2, 3, 3,
  ])
})

Deno.test(`${base}/implicit-unit`, async () => {
  const result: TupleValue = await LoreTest.run(`${base}/implicit-unit.lore`)
  assertEquals(result.lore$type, Tuple.unitType)
})

Deno.test(`${base}/literals`, async () => {
  const result: ListValue<any> = await LoreTest.run(`${base}/literals.lore`)
  assertIsList(result)

  const elements = result.array
  assertEquals(elements[0], 0)
  assertEquals(elements[1], -15)
  assertEquals(elements[2], 0.0)
  assertEquals(elements[3], 1.5)
  assertEquals(elements[4], -1.5)
  assertEquals(elements[5], true)
  assertEquals(elements[6], false)

  const tuples = <ListValue<TupleValue>> elements[7]
  assertIsList(tuples)
  assertTupleEquals(tuples.array[0], [], [])
  assertTupleEquals(tuples.array[1], [0, 'hello', true], [Types.int, Types.string, Types.boolean])
  assertTupleEquals(tuples.array[2], [3, 6, true], [Types.int, Types.int, Types.boolean])

  assertIsFunction(elements[8], Function.type(Tuple.type([Types.int]), Types.int))

  const lists = <ListValue<ListValue<any>>> elements[9]
  assertIsList(lists)
  assertListEquals(lists.array[0], [], Types.nothing)
  assertListEquals(lists.array[1], [1, 2, 3], Types.int)
  assertIsList(lists.array[2], Tuple.type([Types.int, Types.int]))
  assertListForall(lists.array[2], [[1, 2], [3, 4]], (actual, expected) => assertTupleEquals(actual, expected))
  assertListEquals(lists.array[3], [3, 6, false], Sum.type([Types.int, Types.boolean]))
  assertIsList(lists.array[4], List.type(Sum.type([Types.int, Types.string])))
  assertListForall(lists.array[4], [[1, 2], ['test', 'me', 'well man'], ['container']], (actual, expected: Array<number | string>) => {
    assertListEquals(actual, expected)
  })

  const maps = <ListValue<MapValue<any, any>>> elements[10]
  assertIsList(maps)
  assertMapEquals(maps.array[0], [], Types.nothing, Types.nothing)
  assertMapEquals(maps.array[1], [['john', 11], ['martin', 5]], Types.string, Types.int)
  assertIsMap(maps.array[2], Types.int, Map.type(Types.string, Types.string))
  assertMapForall(maps.array[2], [[1, [['test', 'me']]], [2, [['test', 'well man'], ['test2', 'abc']]]], (actual, expected: Array<[string, string]>) => {
    assertMapEquals(actual, expected, Types.string, Types.string)
  })
  assertMapEquals(maps.array[3], [[1, 3], [5, 6], [10, true]], Types.int, Sum.type([Types.int, Types.boolean]))

  const shapes = <ListValue<ShapeValue>> elements[11]
  assertIsList(shapes)
  assertShapeEquals(shapes.array[0], {}, {})
  assertShapeEquals(shapes.array[1], { name: 'John', occupation: 'Salaryman' }, { name: Types.string, occupation: Types.string })
  assertIsShape(shapes.array[2], { part1: Shape.type({ a: Types.int, b: Types.int }), part2: Shape.type({ c: Types.int, a: Types.string }) })
  assertShapeForall(shapes.array[2], { part1: { a: 1, b: 2 }, part2: { c: 3, a: 'hello' } }, (actual, expected) => {
    assertShapeEquals(actual, expected)
  })
})

Deno.test(`${base}/loops`, async () => {
  const result: ListValue<any> = await LoreTest.run(`${base}/loops.lore`)
  assertIsList(result)

  const lists = result.array
  assertListEquals(lists[0], [10, 20, 29.25], Types.real)
  assertListEquals(lists[1], [15, 15, 15], Types.int)
  assertListEquals(lists[2], [11, 4, 41, -1, 16], Types.int)
  assertListEquals(lists[3], [4, 5, 5, 6], Types.int)
})

Deno.test(`${base}/operators`, async () => {
  const result: ListValue<number | boolean> = await LoreTest.run(`${base}/operators.lore`)
  assertListEquals(result, [
    15, 15, 5, 5, 67.5, 2,
    false, true, true, true, false, true,
    true, false, true, true,
  ])
})

Deno.test(`${base}/simple`, async () => {
  const result: ListValue<number> = await LoreTest.run(`${base}/return.lore`)
  assertListEquals(result, [10, 1, 5, 5])
})

Deno.test(`${base}/special-characters`, async () => {
  const result: ListValue<boolean> = await LoreTest.run(`${base}/special-characters.lore`)
  assertListEquals(result, [true, false, false, true])
})

Deno.test(`${base}/strings`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/strings.lore`)
  assertListEquals(result, [
    '',
    '   ',
    '\n',
    '\n\t\r\'$\\',
    'test X\n\t\u0394',
    'my long variable',
    'X my long variable',
    '${quite} SOME $confusion in THAT string',
    'John, you have 11 apples. Please claim your 1000$ apple at the reception.',
    'Martin, you have 5 apples. Please claim your free apple at the reception.',
  ])
})

Deno.test(`${base}/trailing-commmas`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/trailing-commas.lore`)
  assertListEquals(result, [
    'bar',
    'boo',
    'hello',
    '3',
    'Aurifana',
  ])
})
