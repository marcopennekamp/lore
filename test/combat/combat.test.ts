import { LoreTest } from '../base.ts'
import { ObjectValue } from '../../runtime/src/lore/runtime/values/object.ts'
import { assertIsStruct } from '../assertions.ts'

const base = 'combat'

Deno.test(base, async () => {
  const result: ObjectValue = await LoreTest.run(
    `${base}/heroes`, `${base}/monsters`, `${base}/scenarios`, `${base}/simulation`, `${base}/stats`,
    `${base}/status-effects`, `${base}/weapons`,
  )
  assertIsStruct(result, 'SimulationVictory')
  assertIsStruct((result as any).winner, 'Radiant')
})
