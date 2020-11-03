import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { LoreTest } from '../base.ts'
import { Kind } from '../../runtime/src/lore/runtime/types/kinds.ts'
import { TupleValue } from '../../runtime/src/lore/runtime/values/tuple.ts'
import { ObjectValue } from '../../runtime/src/lore/runtime/values/object.ts'
import { assertIsStruct } from '../assertions.ts'

Deno.test('return/parametric', async () => {
  const zombie: ObjectValue = await LoreTest.run('return/parametric')
  assertIsStruct(zombie, 'Zombie')
  assertEquals((zombie as any).name, 'Fred')
  const position: ObjectValue = (zombie as any).position
  assertIsStruct(position, 'Position')
  assertEquals((position as any).x, 1.7)
  assertEquals((position as any).y, 2.5)
  assertEquals((position as any).z, 0.5)
})

Deno.test('return/simple', async () => {
  const result: TupleValue = await LoreTest.run('return/simple')
  assertEquals(result.lore$type.kind, Kind.Product)
  assertEquals(result.elements, [10, 1, 5])
})
