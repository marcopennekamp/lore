import { LoreTest } from '../base.ts'
import { ListValue } from '../../runtime/src/lore/runtime/values/list.ts'
import { assertListEquals } from '../assertions.ts'

const base = 'geometry'

Deno.test(`${base}/area`, async () => {
  const result: ListValue<number> = await LoreTest.run(`${base}/area`, `${base}/geometry`)
  assertListEquals(result, [250, 22.902210444671102, 1540])
})

Deno.test(`${base}/comparison`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/comparison`, `${base}/geometry`)
  assertListEquals(result, ['rect <= box', 'box <= rect', 'rect < circle', 'rect <= circle', 'box < circle', 'box <= circle'])
})
