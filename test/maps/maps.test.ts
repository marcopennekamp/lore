import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertListEqualsUnordered } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('maps/simple', async () => {
  const result: ListValue<string> = await LoreTest.run('maps/simple.lore')
  assertListEqualsUnordered(result, ['a is 0', 'b is 1', 'c is 2'])
})
