import { ListValue } from '../../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../../assertions.ts'
import { LoreTest } from '../../base.ts'

const base = 'features/variables'

Deno.test(`${base}/global`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/global.lore`)
  assertListEquals(result, [
    'John Doe',
    '[2, 3, 4]',
    '[3, 4]',
  ])
})
