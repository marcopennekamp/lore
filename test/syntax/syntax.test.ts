import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { Tuple, TupleValue } from '../../runtime/src/lore/runtime/tuples.ts'
import { LoreTest } from '../base.ts'

Deno.test('syntax/call-line-stretching', async () => {
  const result: TupleValue = await LoreTest.run('syntax/call-line-stretching')
  assertEquals(result.lore$type, Tuple.unitType)
})

Deno.test('syntax/implicit-unit', async () => {
  const result: TupleValue = await LoreTest.run('syntax/implicit-unit')
  assertEquals(result.lore$type, Tuple.unitType)
})
