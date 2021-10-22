import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertIsList, assertIsStruct } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('combat', async () => {
  const result: ListValue<any> = await LoreTest.run(`combat`)

  assertIsList(result)
  const [scenario1, scenario2] = result.elements

  assertIsStruct(scenario1, 'combat.Simulation.Victory')
  const winner1 = (scenario1 as any).winner
  assertIsStruct(winner1, 'combat.combatants.Radiant')
  assertEquals(winner1.name, 'Kaladin')

  assertIsStruct(scenario2, 'combat.Simulation.Stalemate')
})
