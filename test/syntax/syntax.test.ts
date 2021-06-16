import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { Function } from '../../runtime/src/lore/runtime/functions.ts'
import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { Tuple, TupleValue } from '../../runtime/src/lore/runtime/tuples.ts'
import { Types } from '../../runtime/src/lore/runtime/types/types.ts'
import {
  assertIsFunction, assertIsList, assertListEquals, assertMapEquals, assertShapeEquals, assertTupleEquals,
} from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('syntax/call-line-stretching', async () => {
  const result: TupleValue = await LoreTest.run('syntax/call-line-stretching')
  assertEquals(result.lore$type, Tuple.unitType)
})

Deno.test('syntax/implicit-unit', async () => {
  const result: TupleValue = await LoreTest.run('syntax/implicit-unit')
  assertEquals(result.lore$type, Tuple.unitType)
})

Deno.test('syntax/literals', async () => {
  const result: ListValue<any> = await LoreTest.run('syntax/literals')
  assertIsList(result)

  const elements = result.array
  assertEquals(elements[0], 0)
  assertEquals(elements[1], -15)
  assertEquals(elements[2], 0.0)
  assertEquals(elements[3], 1.5)
  assertEquals(elements[4], -1.5)
  assertEquals(elements[5], true)
  assertEquals(elements[6], false)
  assertTupleEquals(elements[7], [0, 'hello', true], [Types.int, Types.string, Types.boolean])
  assertIsFunction(elements[8], Function.type(Tuple.type([Types.int]), Types.int))
  assertListEquals(elements[9], [1, 2, 3], Types.int)
  assertMapEquals(elements[10], [['john', 11], ['martin', 5]], Types.string, Types.int)
  assertShapeEquals(elements[11], { name: 'John', occupation: 'Salaryman' }, { name: Types.string, occupation: Types.string })
})

Deno.test('syntax/strings', async () => {
  const result: ListValue<string> = await LoreTest.run('syntax/strings')
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
