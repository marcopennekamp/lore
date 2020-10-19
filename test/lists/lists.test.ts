import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { ListValue } from '../../runtime/src/lore/runtime/values/list.ts'
import { LoreTest } from '../base.ts'
import { Kind } from '../../runtime/src/lore/runtime/types/kinds.ts'

Deno.test('lists/concat', async () => {
  const result: ListValue<string> = await LoreTest.run('lists/concat')
  assertEquals(result.lore$type.kind, Kind.List)
  assertEquals(
    result.array,
    [
      'Int list: [10]',
      'String/Int list: [10, test]',
      'String/Int/Boolean list: [10, test, true, wow]',
      'String/Int/Boolean list: [10, test, true, wow, 15]',
    ]
  )
})
