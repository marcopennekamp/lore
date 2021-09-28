import { ListValue } from '../../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../../assertions.ts'
import { LoreTest } from '../../base.ts'

const base = 'features/abstract'

Deno.test(`${base}/intersection`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/intersection.lore`)
  assertListEquals(result, ['A', 'C', 'B', 'C', 'C', 'B', 'C', 'B', 'C', 'B', 'A', 'C', 'X1|X2', 'X1|X2', 'X3'])
})

Deno.test(`${base}/partial-specialization`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/partial-specialization.lore`)
  assertListEquals(result, ['A1,B', 'A1,B', 'A2,B', 'A2,B'])
})

Deno.test(`${base}/spell`, async () => {
  const result: ListValue<number> = await LoreTest.run(`${base}/spell.lore`)
  assertListEquals(result, [10, 12, 8, 5, 10, 6, 0, 6, 2])
})

Deno.test(`${base}/sum`, async () => {
  const result: ListValue<number> = await LoreTest.run(`${base}/sum.lore`)
  assertListEquals(result, [3, 4, 4, 5, 5, 1, 2])
})
