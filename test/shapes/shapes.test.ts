import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertListForall } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('shapes/listify', async () => {
  const result: ListValue<ListValue<number>> = await LoreTest.run('shapes/listify')
  const expected = [[1.2, 5], [0, 5.1, 4.8], [1.2, 5], [0, 5.1, 4.8]]
  assertListForall(result, expected, (actual: ListValue<number>, expected: Array<number>) => {
    assertEquals(actual.array, expected)
  })
})
