import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { LoreTest } from '../base.ts'
import { TupleValue } from '../../runtime/src/lore/runtime/values/tuple.ts'
import { unit } from '../../runtime/src/lore/runtime/types/types.ts'

Deno.test('syntax/call-line-stretching', async () => {
  const result: TupleValue = await LoreTest.run('syntax/call-line-stretching')
  assertEquals(result.lore$type, unit)
})
