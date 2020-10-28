import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../runtime/src/lore/runtime/values/list.ts'
import { LoreTest } from '../base.ts'
import { Kind } from '../../runtime/src/lore/runtime/types/kinds.ts'

Deno.test('maps/simple', async () => {
  const result: ListValue<string> = await LoreTest.run('maps/simple')
  assertEquals(result.lore$type.kind, Kind.List)
  assertEquals(result.array.sort((a, b) => a.localeCompare(b)), ['a is 0', 'b is 1', 'c is 2'])
})
