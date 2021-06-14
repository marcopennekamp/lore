import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('inference/function-list', async () => {
  const result: ListValue<string> = await LoreTest.run('inference/function-list')
  assertListEquals(result, ['foo', 'bar', '!foobar!'])
})

Deno.test('inference/greet', async () => {
  const result: ListValue<string> = await LoreTest.run('inference/greet')
  assertListEquals(result, ['Hello, Mr. Smith.', 'Hello, Ms. Chang.'])
})
