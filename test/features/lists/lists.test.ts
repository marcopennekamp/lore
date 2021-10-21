import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../../runtime/src/lore/runtime/lists.ts'
import { StructValue } from '../../../runtime/src/lore/runtime/structs.ts'
import { assertIsStruct, assertListEquals, assertListForall } from '../../assertions.ts'
import { LoreTest } from '../../base.ts'

const base = 'features/lists'

Deno.test(`${base}/append-many`, async () => {
  const result: ListValue<number> = await LoreTest.run(`${base}/append-many.lore`)
  const expected: Array<number> = []
  for (let i = 0; i < 1000; i += 1) {
    expected.push(i)
  }
  assertListEquals(result, expected)
})

Deno.test(`${base}/concat`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/concat.lore`)
  assertListEquals(
    result,
    [
      'Number list: [10]',
      'String/Number list: [10, test]',
      'String/Number/Boolean list: [10, test, true, wow]',
      'String/Number/Boolean list: [10, test, true, wow, 15]',
    ]
  )
})

Deno.test(`${base}/yield`, async () => {
  const result: ListValue<StructValue> = await LoreTest.run(`${base}/yield.lore`)
  assertListForall(result, [2, 10, 20, 24], (actual, expected) => {
    assertIsStruct(actual, 'Gold')
    assertEquals((<any> actual).value, expected)
  })
})
