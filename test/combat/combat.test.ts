import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertIsList, assertIsStruct } from '../assertions.ts'
import { LoreTest } from '../base.ts'

const base = 'combat'

Deno.test(base, async () => {
  const result: ListValue<any> = await LoreTest.run(
    `${base}/heroes.lore`, `${base}/monsters.lore`, `${base}/scenarios.lore`, `${base}/simulation.lore`,
    `${base}/stats.lore`, `${base}/status-effects.lore`, `${base}/weapons.lore`,
  )

  assertIsList(result)
  const [scenario1, scenario2] = result.array

  assertIsStruct(scenario1, 'SimulationVictory')
  const winner1 = (scenario1 as any).winner
  assertIsStruct(winner1, 'Radiant')
  assertEquals(winner1.name, 'Kaladin')

  assertIsStruct(scenario2, 'SimulationStalemate')
})
