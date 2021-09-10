import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('variables/global', async () => {
  const result: ListValue<string> = await LoreTest.run('variables/global.lore')
  assertListEquals(result, [
    'John Doe',
    '[2, 3, 4]',
    '[3, 4]',
  ])
})
