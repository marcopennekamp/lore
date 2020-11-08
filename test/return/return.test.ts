import { LoreTest } from '../base.ts'
import { TupleValue } from '../../runtime/src/lore/runtime/values/tuple.ts'
import { ObjectValue } from '../../runtime/src/lore/runtime/values/object.ts'
import { assertListEquals, assertStructHasValues, assertTupleEquals } from '../assertions.ts'
import { ListValue } from '../../runtime/src/lore/runtime/values/list.ts'

Deno.test('return/identity', async () => {
  const result: ListValue<any> = await LoreTest.run('return/identity')
  assertListEquals(result, [0, 1.2, 'Hello', true])
})

Deno.test('return/parametric', async () => {
  const zombie: ObjectValue = await LoreTest.run('return/parametric')
  assertStructHasValues(zombie, 'Zombie', { name: 'Fred' })
  assertStructHasValues((zombie as any).position, 'Position', { x: 1.7, y: 2.5, z: 0.5 })
})

Deno.test('return/simple', async () => {
  const result: TupleValue = await LoreTest.run('return/simple')
  assertTupleEquals(result, [10, 1, 5])
})
