import { ListValue } from '../../runtime/src/lore/runtime/values/list.ts'
import { LoreTest } from '../base.ts'
import { assertListEquals } from '../assertions.ts'

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
