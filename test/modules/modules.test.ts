import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('modules/homonymous-nested', async () => {
  const result: ListValue<string | number> = await LoreTest.run('modules/homonymous-nested.lore')
  assertListEquals(result, ['Foo', 17])
})

Deno.test('modules/mutual-import', async () => {
  const result: number = await LoreTest.run('modules/mutual-import.lore')
  assertEquals(result, 12)
})

Deno.test('modules/name-resolution', async () => {
  const result: ListValue<number> = await LoreTest.run('modules/name-resolution.lore')
  assertListEquals(result, [13, 5.5, 7])
})
