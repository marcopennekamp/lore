import { List, ListValue } from '../../../runtime/src/lore/runtime/lists.ts'
import { StructValue } from '../../../runtime/src/lore/runtime/structs.ts'
import { TupleValue } from '../../../runtime/src/lore/runtime/tuples.ts'
import { Types } from '../../../runtime/src/lore/runtime/types/types.ts'
import {
  assertIsList, assertIsTuple, assertListEquals, assertListForall, assertStructHasValues,
} from '../../assertions.ts'
import { LoreTest } from '../../base.ts'

const base = 'features/functions'

Deno.test(`${base}/constructor`, async () => {
  const result: ListValue<StructValue> = await LoreTest.run(`${base}/constructor.lore`)
  assertIsList(result)
  assertStructHasValues(result.array[0], 'Person', { name: 'Victor', age: 21 })
  assertStructHasValues(result.array[1], 'Company', { name: 'Victor', worth: 21 })
})

Deno.test(`${base}/filter-curried`, async () => {
  const result: ListValue<ListValue<number>> = await LoreTest.run(`${base}/filter-curried.lore`)
  assertListForall(
    result,
    [[2, 4], [4, 8, 6], [], []],
    (actual, expected) => assertListEquals(actual, expected),
  )
})

Deno.test(`${base}/greet`, async () => {
  const result: ListValue<string> = await LoreTest.run(`${base}/greet.lore`)
  assertListEquals(result, ['Hello, Mr. Smith.', 'Hello, Ms. Chang.'])
})

Deno.test(`${base}/map`, async () => {
  const result: TupleValue = await LoreTest.run(`${base}/map.lore`)
  assertIsTuple(result, [List.type(Types.number), List.type(Types.string)])

  const numbers = result.elements[0]
  assertListEquals(numbers, [4, 5, 6, 7, 8])

  const names = result.elements[1]
  assertListEquals(names, ['Alpha', 'Beta', 'Gamma'])
})

Deno.test(`${base}/multi-function-value`, async () => {
  const result: ListValue<number> = await LoreTest.run(`${base}/multi-function-value.lore`)
  assertListEquals(result, [6.0, 15.0, 10.5, 38.5])
})
