import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('options/operations', async () => {
  const result: ListValue<string> = await LoreTest.run('options/operations.lore')
  assertListEquals(result, [
    'hello options',
    'fallback',
    'test',
    'fallback',

    'false',
    'true',
    'false',
    'true',

    'true',
    'false',
    'true',
    'false',
  ])
})
