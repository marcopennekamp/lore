import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../runtime/src/lore/runtime/values/list.ts'
import { LoreTest } from '../base.ts'
import { Kind } from '../../runtime/src/lore/runtime/types/kinds.ts'

Deno.test('structs/independent', async () => {
  const result: ListValue<number> = await LoreTest.run('structs/independent')
  assertEquals(result.lore$type.kind, Kind.List)
  assertEquals(result.array, ['CA', 'CB'])
})
