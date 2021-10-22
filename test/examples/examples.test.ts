import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { Lists, ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { BasicType } from '../../runtime/src/lore/runtime/types/basic-types.ts'
import { assertIsList, assertListEquals } from '../assertions.ts'
import { LoreTest } from '../base.ts'

const base = 'examples'

Deno.test(`${base}/airborne`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/airborne.lore`)
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

Deno.test(`${base}/bunsnatcher`, async () => {
  const result: ListValue<number> = await LoreTest.run(`${base}/bunsnatcher.lore`)
  assertIsList(result)
  assertEquals(
    result.elements.map(x => Number.parseFloat(x.toFixed(3))).toJS(),
    [1.248, 0.6, 3.78, 1.2, 1.8, 0.96, 2.16, 1.8, 2.016, 0.08],
  )
})

Deno.test(`${base}/savable`, async () => {
  const result: ListValue<number> = await LoreTest.run(`${base}/savable.lore`)

  // Because of how run-time list types are handled, the resulting list will have the type `[Any]`, given how Positions
  // are saved. In essence, because at compile time the list construction `[['Position'], save(pos.x), save(pos.y)]`
  // has the type `[[Any]]`, the resulting list's type will be `[Any]`.
  assertListEquals(result, ['Character', 'Position', 1.5, 2.7, 'ExternalPhysics', 42], BasicType.any)
})
