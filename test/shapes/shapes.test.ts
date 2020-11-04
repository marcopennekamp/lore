import { LoreTest } from '../base.ts'
import { ListValue } from '../../runtime/src/lore/runtime/values/list.ts'
import { assertListEquals } from '../assertions.ts'

Deno.test('shapes/area', async () => {
  const result: ListValue<number> = await LoreTest.run('shapes/area', 'shapes/shapes')
  assertListEquals(result, [250, 22.902210444671102, 1540])
})

Deno.test('shapes/comparisons', async () => {
  const result: ListValue<number> = await LoreTest.run('shapes/comparisons', 'shapes/shapes')
  assertListEquals(result, ['rect <= box', 'box <= rect', 'rect < circle', 'rect <= circle', 'box < circle', 'box <= circle'])
})
