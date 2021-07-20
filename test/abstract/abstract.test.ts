import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('abstract/intersection', async () => {
  const result: ListValue<string> = await LoreTest.run('abstract/intersection.lore')
  assertListEquals(result, ['A', 'C', 'B', 'C', 'C', 'B', 'C', 'B', 'C', 'B', 'A', 'C', 'X1|X2', 'X1|X2', 'X3'])
})

Deno.test('abstract/partial-specialization', async () => {
  const result: ListValue<string> = await LoreTest.run('abstract/partial-specialization.lore')
  assertListEquals(result, ['A1,B', 'A1,B', 'A2,B', 'A2,B'])
})

Deno.test('abstract/sum', async () => {
  const result: ListValue<number> = await LoreTest.run('abstract/sum.lore')
  assertListEquals(result, [3, 4, 4, 5, 5, 1, 2])
})
