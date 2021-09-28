import { ListValue } from '../../../runtime/src/lore/runtime/lists.ts'
import { StructValue } from '../../../runtime/src/lore/runtime/structs.ts'
import { assertListEquals, assertListForall, assertStructHasValues } from '../../assertions.ts'
import { LoreTest } from '../../base.ts'

const base = 'features/structs'

Deno.test(`${base}/objects`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/objects.lore`)
  assertListEquals(result, ['empty', '1', '2', '4', 'empty', '5', 'empty', '5', 'func', '1'])
})

Deno.test(`${base}/position`, async () => {
  const result: ListValue<StructValue> = await LoreTest.run(`${base}/position.lore`)
  const expected = [{ x: 1, y: 2, z: 3 }, { x: 0, y: 5, z: 0 }, { x: 20, y: 10, z: 0 }, { x: 7, y: 0, z: 0}]
  assertListForall(result, expected, (actual, expected) => assertStructHasValues(actual, 'Position', expected))
})

Deno.test(`${base}/runtime-type-equality`, async () => {
  const result: ListValue<boolean> = await LoreTest.run(`${base}/runtime-type-equality.lore`)
  assertListEquals(result, [true, true, false, false, true, true, false, false])
})
