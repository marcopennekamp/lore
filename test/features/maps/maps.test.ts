import { ListValue } from '../../../runtime/src/lore/runtime/lists.ts'
import { assertListEqualsUnordered } from '../../assertions.ts'
import { LoreTest } from '../../base.ts'

const base = 'features/maps'

Deno.test(`${base}/simple`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/simple.lore`)
  assertListEqualsUnordered(result, ['a is 0', 'b is 1', 'c is 2'])
})
