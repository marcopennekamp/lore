import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('calculator', async () => {
  const result: ListValue<number> = await LoreTest.run('calculator')
  assertListEquals(result, [0, 3, 7, 0.5, -35])
})
