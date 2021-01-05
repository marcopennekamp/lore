import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('traits/airborne', async () => {
  const result: ListValue<string> = await LoreTest.run('traits/airborne')
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
