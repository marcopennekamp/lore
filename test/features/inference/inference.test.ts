import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../../assertions.ts'
import { LoreTest } from '../../base.ts'

const base = 'features/inference'

Deno.test(`${base}/ascription`, async () => {
  const result: string = await LoreTest.run(`${base}/ascription.lore`)
  assertEquals(result, 'Some(mouse)')
})

Deno.test(`${base}/function_list`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/function_list.lore`)
  assertListEquals(result, ['foo', 'bar', '!foobar!'])
})

Deno.test(`${base}/function_list_append`, async () => {
  const result: ListValue<boolean> = await LoreTest.run(`${base}/function_list_append.lore`)
  assertListEquals(result, [false, true, true, false, false, false, false, true])
})

Deno.test(`${base}/sum_list`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/sum_list.lore`)
  assertListEquals(result, ['cat', 'dog', 'cat'])
})

Deno.test(`${base}/wrapper`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/wrapper.lore`)
  assertListEquals(result, ['Hey!', 'Hey! Hey!', 'Hey! Hey! Hey!'])
})

Deno.test(`${base}/wrapper_alias`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/wrapper_alias.lore`)
  assertListEquals(result, ['Hey!', 'Hey! Hey! Hey!', 'Hey! Hey! Hey! Hey! Hey!'])
})
