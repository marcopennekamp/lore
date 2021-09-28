import { ListValue } from '../../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../../assertions.ts'
import { LoreTest } from '../../base.ts'

const base = 'pyramid/options'

Deno.test(`${base}/operations`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/operations.lore`)
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
