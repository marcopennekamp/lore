import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('schemas/array', async () => {
  const result: ListValue<number> = await LoreTest.run('schemas/array.lore')
  assertListEquals(result, [2, 3, 4])
})
