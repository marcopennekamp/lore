import { LoreTest } from '../base.ts'
import { TupleValue } from '../../runtime/src/lore/runtime/values/tuple.ts'
import { ObjectValue } from '../../runtime/src/lore/runtime/values/object.ts'
import { assertStructHasValues, assertTupleEquals } from '../assertions.ts'

Deno.test('return/parametric', async () => {
  const zombie: ObjectValue = await LoreTest.run('return/parametric')
  assertStructHasValues(zombie, 'Zombie', { name: 'Fred' })
  assertStructHasValues((zombie as any).position, 'Position', { x: 1.7, y: 2.5, z: 0.5 })
})

Deno.test('return/simple', async () => {
  const result: TupleValue = await LoreTest.run('return/simple')
  assertTupleEquals(result, [10, 1, 5])
})
