import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { StructValue } from '../../runtime/src/lore/runtime/structs.ts'
import { assertIsStruct, assertListEquals, assertListForall } from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('lists/concat', async () => {
  const result: ListValue<string> = await LoreTest.run('lists/concat')
  assertListEquals(
    result,
    [
      'Int list: [10]',
      'String/Int list: [10, test]',
      'String/Int/Boolean list: [10, test, true, wow]',
      'String/Int/Boolean list: [10, test, true, wow, 15]',
    ]
  )
})

Deno.test('lists/yield', async () => {
  const result: ListValue<StructValue> = await LoreTest.run('lists/yield')
  assertListForall(result, [2, 10, 20, 24], (actual, expected) => {
    assertIsStruct(actual, 'Gold')
    assertEquals((<any> actual).value, expected)
  })
})
