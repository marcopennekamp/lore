import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { Function } from '../../../runtime/src/lore/runtime/functions.ts'
import { Lists, ListValue } from '../../../runtime/src/lore/runtime/lists.ts'
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

  const lists = result.elements
  assertListEquals(lists.get(0), [10], Types.number)
  assertListEquals(lists.get(1), [5.5], Types.number)
  assertListEquals(lists.get(2), ['hello'], Types.string)
  assertListEquals(lists.get(3), [12, 14.5, 10], Types.number)
  assertListEquals(lists.get(4), [12, 14.5, 5.5], Types.number)
  assertListEquals(lists.get(5), [12, 14.5, 'hello'], Sum.type([Types.number, Types.string]))
  assertListEquals(lists.get(6), [44, -5, 7, 10], Types.number)
  assertListEquals(lists.get(7), [44, -5, 7, 5.5], Types.number)
  assertListEquals(lists.get(8), [44, -5, 7, 'hello'], Sum.type([Types.number, Types.string]))
  assertListEquals(lists.get(9), [44, -5, 7, 'hello', 'world'], Sum.type([Types.number, Types.string]))
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
    1, 3, 2, 2,
  ])
})

Deno.test(`${base}/implicit-unit`, async () => {
  const result: TupleValue = await LoreTest.run(`${base}/implicit-unit.lore`)
  assertEquals(result.lore$type, Tuple.unitType)
})

Deno.test(`${base}/literals`, async () => {
  const result: ListValue<any> = await LoreTest.run(`${base}/literals.lore`)
  assertIsList(result)

  const elements = result.elements
  assertEquals(elements.get(0), 0)
  assertEquals(elements.get(1), -15)
  assertEquals(elements.get(2), 0.0)
  assertEquals(elements.get(3), 1.5)
  assertEquals(elements.get(4), -1.5)
  assertEquals(elements.get(5), true)
  assertEquals(elements.get(6), false)

  const tuples = <ListValue<TupleValue>> elements.get(7)
  assertIsList(tuples)
  assertTupleEquals(tuples.elements.get(0), [], [])
  assertTupleEquals(tuples.elements.get(1), [0, 'hello', true], [Types.number, Types.string, Types.boolean])
  assertTupleEquals(tuples.elements.get(2), [3, 6, true], [Types.number, Types.number, Types.boolean])

  assertIsFunction(elements.get(8), Function.type(Tuple.type([Types.number]), Types.number))

  const lists = <ListValue<ListValue<any>>> elements.get(9)
  assertIsList(lists)
  assertListEquals(lists.elements.get(0), [], Types.nothing)
  assertListEquals(lists.elements.get(1), [1, 2, 3], Types.number)
  assertIsList(lists.elements.get(2), Tuple.type([Types.number, Types.number]))
  assertListForall(lists.elements.get(2), [[1, 2], [3, 4]], (actual, expected) => assertTupleEquals(actual, expected))
  assertListEquals(lists.elements.get(3), [3, 6, false], Sum.type([Types.number, Types.boolean]))
  assertIsList(lists.elements.get(4), Lists.type(Sum.type([Types.number, Types.string])))
  assertListForall(lists.elements.get(4), [[1, 2], ['test', 'me', 'well man'], ['container']], (actual, expected: Array<number | string>) => {
    assertListEquals(actual, expected)
  })

  const maps = <ListValue<MapValue<any, any>>> elements.get(10)
  assertIsList(maps)
  assertMapEquals(maps.elements.get(0), [], Types.nothing, Types.nothing)
  assertMapEquals(maps.elements.get(1), [['john', 11], ['martin', 5]], Types.string, Types.number)
  assertIsMap(maps.elements.get(2), Types.number, Map.type(Types.string, Types.string))
  assertMapForall(maps.elements.get(2), [[1, [['test', 'me']]], [2, [['test', 'well man'], ['test2', 'abc']]]], (actual, expected: Array<[string, string]>) => {
    assertMapEquals(actual, expected, Types.string, Types.string)
  })
  assertMapEquals(maps.elements.get(3), [[1, 3], [5, 6], [10, true]], Types.number, Sum.type([Types.number, Types.boolean]))

  const shapes = <ListValue<ShapeValue>> elements.get(11)
  assertIsList(shapes)
  assertShapeEquals(shapes.elements.get(0), {}, {})
  assertShapeEquals(shapes.elements.get(1), { name: 'John', occupation: 'Salaryman' }, { name: Types.string, occupation: Types.string })
  assertIsShape(shapes.elements.get(2), { part1: Shape.type({ a: Types.number, b: Types.number }), part2: Shape.type({ c: Types.number, a: Types.string }) })
  assertShapeForall(shapes.elements.get(2), { part1: { a: 1, b: 2 }, part2: { c: 3, a: 'hello' } }, (actual, expected) => {
    assertShapeEquals(actual, expected)
  })
})

Deno.test(`${base}/loops`, async () => {
  const result: ListValue<any> = await LoreTest.run(`${base}/loops.lore`)
  assertIsList(result)

  const lists = result.elements
  assertListEquals(lists.get(0), [10, 20, 29.25], Types.number)
  assertListEquals(lists.get(1), [15, 15, 15], Types.number)
  assertListEquals(lists.get(2), [11, 4, 41, -1, 16], Types.number)
  assertListEquals(lists.get(3), [4, 5, 5, 6], Types.number)
})

Deno.test(`${base}/operators`, async () => {
  const result: ListValue<number | boolean> = await LoreTest.run(`${base}/operators.lore`)
  assertListEquals(result, [
    15, 15, 5, 5, 67.5, 2,
    false, true, true, true, false, true,
    true, false, true, true,
  ])
})

Deno.test(`${base}/return`, async () => {
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

Deno.test(`${base}/trailing-commas`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/trailing-commas.lore`)
  assertListEquals(result, [
    'bar',
    'boo',
    'hello',
    '3',
    'Aurifana',
  ])
})
