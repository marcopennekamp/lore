import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../runtime/src/lore/runtime/values/list.ts'
import { LoreTest } from '../base.ts'
import { Kind } from '../../runtime/src/lore/runtime/types/kinds.ts'
import { ObjectValue } from '../../runtime/src/lore/runtime/values/object.ts'
import { assertIsStruct } from '../assertions.ts'

Deno.test('structs/independent', async () => {
  const result: ListValue<number> = await LoreTest.run('structs/independent')
  assertEquals(result.lore$type.kind, Kind.List)
  assertEquals(result.array, ['CA', 'CB'])
})

Deno.test('structs/position', async () => {
  const result: ListValue<ObjectValue> = await LoreTest.run('structs/position')
  assertEquals(result.lore$type.kind, Kind.List)
  const positions = result.array

  assertPosition(positions[0], 1, 2, 3)
  assertPosition(positions[1], 0, 5, 0)
  assertPosition(positions[2], 20, 10, 0)
  assertPosition(positions[3], 7, 0, 0)

  function assertPosition(position: ObjectValue, x: number, y: number, z: number) {
    assertIsStruct(position, 'Position')
    assertEquals((position as any).x, x)
    assertEquals((position as any).y, y)
    assertEquals((position as any).z, z)
  }
})
