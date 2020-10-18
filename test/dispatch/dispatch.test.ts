import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../runtime/src/lore/runtime/values/list.ts'
import { LoreTest } from '../base.ts'
import { Kind } from '../../runtime/src/lore/runtime/types/kinds.ts'

Deno.test('dispatch/hello-name', async () => {
  const result: ListValue<string> = await LoreTest.run('dispatch/hello-name')
  assertEquals(result.lore$type.kind, Kind.List)
  assertEquals(result.array, ['Hello, world.', 'Hello, anonymous #5.', 'Hello, marco.', 'Hello, console.', 'Hello, anonymous #100.'])
})
