import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../assertions.ts'
import { LoreTest } from '../base.ts'

const base = 'introspection'

Deno.test(`${base}/subtyping`, async () => {
  const result: ListValue<boolean> = await LoreTest.run(`${base}/subtyping.lore`)
  assertListEquals(result, [true, false, true, false, false, true, false])
})
