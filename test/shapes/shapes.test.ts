import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { LoreTest } from '../base.ts'
import { Kind } from '../../runtime/src/lore/runtime/types/kinds.ts'
import { ListValue } from '../../runtime/src/lore/runtime/values/list.ts'

Deno.test('shapes/area: evaluates to (250, 22.9, 1540)', async () => {
  const result: ListValue<number> = await LoreTest.run('shapes/area', 'shapes/shapes')
  assertEquals(result.lore$type.kind, Kind.List)
  assertEquals(result.array, [250, 22.902210444671102, 1540])
})
