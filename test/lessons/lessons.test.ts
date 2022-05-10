import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { assertListEquals, assertListEpsilonEquals } from '../assertions.ts'
import { LoreTest } from '../base.ts'

const base = 'lessons'

Deno.test(`${base}/fibonacci`, async () => {
  const result: number = await LoreTest.run(`${base}/fibonacci.lore`)
  assertEquals(result, 55)
})

Deno.test(`${base}/hello_name`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/hello_name.lore`)
  assertListEquals(result, [
    'Hello, world.',
    'Hello, anonymous #5.',
    'Hello, marco.',
    'Hello, console.',
    'Hello, anonymous #42.'
  ])
})

Deno.test(`${base}/resistor`, async () => {
  const result: ListValue<number> = await LoreTest.run(`${base}/resistor.lore`)
  assertListEpsilonEquals(result, [2565000, 2835000, 554.4, 565.6, 21945, 22055, 446.5, 493.5, 67.966, 68.034, 532.8, 799.2])
})

Deno.test(`${base}/rna`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/rna.lore`)
  assertListEquals(result, ['u', 'a', 'c', 'g', 'ugcaccagaauu'])
})

Deno.test(`${base}/status`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/status.lore`)
  assertListEquals(result, ['Success!', 'Failure!', 'Success!', 'Failure!', 'Success!'])
})
