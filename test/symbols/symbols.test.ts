import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('symbols/status', async () => {
  const result: ListValue<string> = await LoreTest.run('symbols/status.lore')
  assertListEquals(result, ['Success!', 'Failure!', 'Success!', 'Failure!'])
})
