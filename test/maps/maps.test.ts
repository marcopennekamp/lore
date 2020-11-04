import { ListValue } from '../../runtime/src/lore/runtime/values/list.ts'
import { LoreTest } from '../base.ts'
import { assertListEqualsUnordered } from '../assertions.ts'

Deno.test('maps/simple', async () => {
  const result: ListValue<string> = await LoreTest.run('maps/simple')
  assertListEqualsUnordered(result, ['a is 0', 'b is 1', 'c is 2'])
})
