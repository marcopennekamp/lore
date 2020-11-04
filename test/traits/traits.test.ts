import { ListValue } from '../../runtime/src/lore/runtime/values/list.ts'
import { LoreTest } from '../base.ts'
import { assertListEquals } from '../assertions.ts'

Deno.test('traits/airborne', async () => {
  const result: ListValue<number> = await LoreTest.run('traits/airborne')
  assertListEquals(result, [
    'Raven == Raven',
    'Raven == Model Plane',
    'Dragon == Dragon',
    'Dragon == Cessna',
    'Crow == Crow',
    'Crow == Model Plane',
    'B-2 Spirit == B-2 Spirit',
    'Cessna == Cessna',
    'Model Plane == Model Plane'
  ])
})
