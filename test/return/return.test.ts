import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { LoreTest } from '../base.ts'
import { Kind } from '../../runtime/src/lore/runtime/types/kinds.ts'
import { TupleValue } from '../../runtime/src/lore/runtime/values/tuple.ts'
import { ObjectValue } from '../../runtime/src/lore/runtime/values/object.ts'

Deno.test('return/simple: evaluates to (10, 1, 5)', async () => {
  const result: TupleValue = await LoreTest.run('return/simple')
  assertEquals(result.lore$type.kind, Kind.Product)
  assertEquals(result.elements, [10, 1, 5])
})

Deno.test('return/parametric: evaluates to a zombie with the correct name and position', async () => {
  const zombie: ObjectValue = await LoreTest.run('return/parametric')
  assertEquals(zombie.lore$type.kind, Kind.Struct)
  assertEquals(zombie.lore$type.schema.name, 'Zombie')
  assertEquals((zombie as any).name, 'Fred')
  const position: ObjectValue = (zombie as any).position
  assertEquals(position.lore$type.kind, Kind.Struct)
  assertEquals(position.lore$type.schema.name, 'Position')
  assertEquals((position as any).x, 1.7)
  assertEquals((position as any).y, 2.5)
  assertEquals((position as any).z, 0.5)
})
