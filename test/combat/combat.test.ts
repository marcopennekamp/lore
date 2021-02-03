import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertIsList, assertIsStruct } from '../assertions.ts'
import { LoreTest } from '../base.ts'

const base = 'combat'

Deno.test(base, async () => {
  const result: ListValue<any> = await LoreTest.run(
    `${base}/heroes`, `${base}/monsters`, `${base}/scenarios`, `${base}/simulation`, `${base}/stats`,
    `${base}/status-effects`, `${base}/weapons`,
  )

  assertIsList(result)
  const [scenario1, scenario2] = result.array

  assertIsStruct(scenario1, 'SimulationVictory')
  const winner1 = (scenario1 as any).winner
  assertIsStruct(winner1, 'Radiant')
  assertEquals(winner1.name, 'Kaladin')

  assertIsStruct(scenario2, 'SimulationStalemate')
})
