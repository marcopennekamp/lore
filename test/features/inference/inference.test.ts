import { ListValue } from '../../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../../assertions.ts'
import { LoreTest } from '../../base.ts'

const base = 'features/inference'

Deno.test(`${base}/function-list`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/function-list.lore`)
  assertListEquals(result, ['foo', 'bar', '!foobar!'])
})

Deno.test(`${base}/greet`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/greet.lore`)
  assertListEquals(result, ['Hello, Mr. Smith.', 'Hello, Ms. Chang.'])
})

Deno.test(`${base}/wrapper`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/wrapper.lore`)
  assertListEquals(result, ['Hey!', 'Hey! Hey!', 'Hey! Hey! Hey!'])
})