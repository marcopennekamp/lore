import { ListValue } from '../../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../../assertions.ts'
import { LoreTest } from '../../base.ts'

const base = 'features/inference'

Deno.test(`${base}/function-list`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/function-list.lore`)
  assertListEquals(result, ['foo', 'bar', '!foobar!'])
})

Deno.test(`${base}/sum-list`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/sum-list.lore`)
  assertListEquals(result, ['cat', 'dog', 'cat'])
})

Deno.test(`${base}/wrapper`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/wrapper.lore`)
  assertListEquals(result, ['Hey!', 'Hey! Hey!', 'Hey! Hey! Hey!'])
})
