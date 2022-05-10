import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../../assertions.ts'
import { LoreTest } from '../../base.ts'

const base = 'features/modules'

Deno.test(`${base}/companions`, async () => {
  const result: ListValue<number> = await LoreTest.run(`${base}/companions.lore`)
  assertListEquals(result, [0, 1, 2, 3, 4, 5, 6, 7])
})

Deno.test(`${base}/homonymous_nested`, async () => {
  const result: ListValue<string | number> = await LoreTest.run(`${base}/homonymous_nested.lore`)
  assertListEquals(result, ['Foo', 17])
})

Deno.test(`${base}/mutual_import`, async () => {
  const result: number = await LoreTest.run(`${base}/mutual_import.lore`)
  assertEquals(result, 12)
})

Deno.test(`${base}/name_resolution`, async () => {
  const result: ListValue<number> = await LoreTest.run(`${base}/name_resolution.lore`)
  assertListEquals(result, [13, 5.5, 7])
})
