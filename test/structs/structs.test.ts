import { ListValue } from '../../runtime/src/lore/runtime/values/list.ts'
import { LoreTest } from '../base.ts'
import { ObjectValue } from '../../runtime/src/lore/runtime/values/object.ts'
import { assertListEquals, assertListForall, assertStructHasValues } from '../assertions.ts'

Deno.test('structs/independent', async () => {
  const result: ListValue<string> = await LoreTest.run('structs/independent')
  assertListEquals(result, ['CA', 'CB'])
})

Deno.test('structs/position', async () => {
  const result: ListValue<ObjectValue> = await LoreTest.run('structs/position')
  const expected = [{ x: 1, y: 2, z: 3 }, { x: 0, y: 5, z: 0 }, { x: 20, y: 10, z: 0 }, { x: 7, y: 0, z: 0}]
  assertListForall(result, expected, (actual, expected) => assertStructHasValues(actual, 'Position', expected))
})

Deno.test('structs/runtime-type-equality', async () => {
  const result: ListValue<boolean> = await LoreTest.run('structs/runtime-type-equality')
  assertListEquals(result, [true, true, false, false, true, true, false, false])
})
