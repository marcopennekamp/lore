import { ListValue } from '../../runtime/src/lore/runtime/lists.ts'
import { StructValue } from '../../runtime/src/lore/runtime/structs.ts'
import { TupleValue } from '../../runtime/src/lore/runtime/tuples.ts'
import {
  assertIsList, assertIsTuple, assertListEquals, assertListForall, assertStructHasValues,
} from '../assertions.ts'
import { LoreTest } from '../base.ts'

Deno.test('functions/constructor', async () => {
  const result: ListValue<StructValue> = await LoreTest.run('functions/constructor')
  assertIsList(result)
  assertStructHasValues(result.array[0], 'Person', { name: 'Victor', age: 21 })
  assertStructHasValues(result.array[1], 'Company', { name: 'Victor', worth: 21 })
})

Deno.test('functions/filter-curried', async () => {
  const result: ListValue<ListValue<number>> = await LoreTest.run('functions/filter-curried')
  assertListForall(
    result,
    [[2, 4], [4, 8, 6], [], []],
    (actual, expected) => assertListEquals(actual, expected),
  )
})

Deno.test('functions/map', async () => {
  const result: TupleValue = await LoreTest.run('functions/map')
  assertIsTuple(result, 2)

  const numbers = result.elements[0]
  assertListEquals(numbers, [4, 5, 6, 7, 8])

  const names = result.elements[1]
  assertListEquals(names, ['Alpha', 'Beta', 'Gamma'])
})

Deno.test('functions/multi-function-value', async () => {
  const result: ListValue<number> = await LoreTest.run('functions/multi-function-value')
  assertListEquals(result, [6.0, 15.0, 10.5, 38.5])
})
