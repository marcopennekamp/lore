import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertIsList, assertListEquals, assertListForall } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('shapes/bunsnatcher', async () => {
  const result: ListValue<number> = await LoreTest.run('shapes/bunsnatcher')
  assertIsList(result)
  assertEquals(
    result.array.map(x => Number.parseFloat(x.toFixed(3))),
    [1.248, 0.6, 3.78, 1.2, 1.8, 0.96, 2.16, 1.8, 2.016, 0.08],
  )
})

Deno.test('shapes/errors', async () => {
  const result: ListValue<string> = await LoreTest.run('shapes/errors')
  assertListEquals(result, ['I am a result!', 'I am another result!'])
})

Deno.test('shapes/listify', async () => {
  const result: ListValue<ListValue<number>> = await LoreTest.run('shapes/listify')
  const expected = [
    [1.2, 5], [0, 5.1, 4.8], [1.2, 5], [0, 5.1, 4.8],
    [1.2, 5], [0, 5.1, 4.8], [1.2, 5], [0, 5.1, 4.8]
  ]
  assertListForall(result, expected, (actual: ListValue<number>, expected: Array<number>) => {
    assertEquals(actual.array, expected)
  })
})

Deno.test('shapes/options', async () => {
  const result: string = await LoreTest.run('shapes/options')
  assertEquals(result, 'Your dog Robby barks at a MADNESS-INDUCING volume showing sharp teeth!')
})
