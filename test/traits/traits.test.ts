import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../runtime/src/lore/runtime/values/list.ts'
import { LoreTest } from '../base.ts'
import { Kind } from '../../runtime/src/lore/runtime/types/kinds.ts'

Deno.test('traits/airborne', async () => {
  const result: ListValue<number> = await LoreTest.run('traits/airborne')
  assertEquals(result.lore$type.kind, Kind.List)
  assertEquals(result.array, [
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
