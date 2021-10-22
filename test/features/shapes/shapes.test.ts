import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals, assertListForall } from '../../assertions.ts'
import { LoreTest } from '../../base.ts'

const base = 'features/shapes'

Deno.test(`${base}/barks`, async () => {
  const result: string = await LoreTest.run(`${base}/barks.lore`)
  assertEquals(result, 'Your dog Raider barks at a MADNESS-INDUCING volume showing sharp teeth!')
})

Deno.test(`${base}/errors`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/errors.lore`)
  assertListEquals(result, ['I am a result!', 'I am another result!'])
})

Deno.test(`${base}/listify`, async () => {
  const result: ListValue<ListValue<number>> = await LoreTest.run(`${base}/listify.lore`)
  const expected = [
    [1.2, 5], [0, 5.1, 4.8], [1.2, 5], [0, 5.1, 4.8],
    [1.2, 5], [0, 5.1, 4.8], [1.2, 5], [0, 5.1, 4.8]
  ]
  assertListForall(result, expected, (actual: ListValue<number>, expected: Array<number>) => {
    assertListEquals(actual, expected)
  })
})

Deno.test(`${base}/mut`, async () => {
  const result: ListValue<number> = await LoreTest.run(`${base}/mut.lore`)
  assertListEquals(result, [0, 1, 3])
})
