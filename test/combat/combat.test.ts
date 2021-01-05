import { StructValue } from '../../runtime/src/lore/runtime/structs.ts'
import { assertIsStruct } from '../assertions.ts'
import { LoreTest } from '../base.ts'

const base = 'combat'

Deno.test(base, async () => {
  const result: StructValue = await LoreTest.run(
    `${base}/heroes`, `${base}/monsters`, `${base}/scenarios`, `${base}/simulation`, `${base}/stats`,
    `${base}/status-effects`, `${base}/weapons`,
  )
  assertIsStruct(result, 'SimulationVictory')
  assertIsStruct((result as any).winner, 'Radiant')
})
