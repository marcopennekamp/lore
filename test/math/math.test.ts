import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { TupleValue } from '../../runtime/src/lore/runtime/tuples.ts'
import { assertListEquals, assertListForall, assertTupleEquals } from '../assertions.ts'
import { LoreTest } from '../base.ts'

const base = 'math'

Deno.test(`${base}/combinations`, async () => {
  const result: ListValue<TupleValue> = await LoreTest.run(`${base}/combinations.lore`)
  const expected = [
    [1, 1], [1, 2], [1, 3], [1, 4],
    [2, 1], [2, 2], [2, 3], [2, 4],
    [3, 1], [3, 2], [3, 3], [3, 4],
    [4, 1], [4, 2], [4, 3], [4, 4],
  ]
  assertListForall(result, expected, (actual: TupleValue, expected: Array<number>) => {
    assertTupleEquals(actual, expected)
  })
})

Deno.test(`${base}/double`, async () => {
  const result: ListValue<number> = await LoreTest.run(`${base}/double.lore`)
  assertListEquals(result, [0, -4, 5, 12, 44])
})

Deno.test('math/min-max', async () => {
  const result: ListValue<number> = await LoreTest.run('math/min-max.lore')
  assertListEquals(result, [-5, 0, 1.8, 1.2, 1])
})

Deno.test(`${base}/naivemethic`, async () => {
  const result: number = await LoreTest.run(`${base}/naivemethic.lore`)
  assertEquals(result, 60)
})

Deno.test(`${base}/pow`, async () => {
  const result: ListValue<number> = await LoreTest.run(`${base}/pow.lore`)
  assertListEquals(result, [1, 0.25, 16, 9, 0, 16, 256])
})
