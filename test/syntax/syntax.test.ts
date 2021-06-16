import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { Tuple, TupleValue } from '../../runtime/src/lore/runtime/tuples.ts'
import { assertListEquals } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('syntax/call-line-stretching', async () => {
  const result: TupleValue = await LoreTest.run('syntax/call-line-stretching')
  assertEquals(result.lore$type, Tuple.unitType)
})

Deno.test('syntax/implicit-unit', async () => {
  const result: TupleValue = await LoreTest.run('syntax/implicit-unit')
  assertEquals(result.lore$type, Tuple.unitType)
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
