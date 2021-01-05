import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { StructValue } from '../../runtime/src/lore/runtime/structs.ts'
import { TupleValue } from '../../runtime/src/lore/runtime/tuples.ts'
import { assertListEquals, assertStructHasValues, assertTupleEquals } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('return/identity', async () => {
  const result: ListValue<any> = await LoreTest.run('return/identity')
  assertListEquals(result, [0, 1.2, 'Hello', true])
})

Deno.test('return/parametric', async () => {
  const zombie: StructValue = await LoreTest.run('return/parametric')
  assertStructHasValues(zombie, 'Zombie', { name: 'Fred' })
  assertStructHasValues((zombie as any).position, 'Position', { x: 1.7, y: 2.5, z: 0.5 })
})

Deno.test('return/simple', async () => {
  const result: TupleValue = await LoreTest.run('return/simple')
  assertTupleEquals(result, [10, 1, 5])
})
