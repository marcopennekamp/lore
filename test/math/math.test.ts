import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { LoreTest } from '../base.ts'
import { Kind } from '../../runtime/src/lore/runtime/types/kinds.ts'
import { ListValue } from '../../runtime/src/lore/runtime/values/list.ts'

Deno.test('math/min-max: evaluates to [-5, 0, 1.8, 1.2, 1]', async () => {
  const result: ListValue<number> = await LoreTest.run('math/min-max')
  assertEquals(result.lore$type.kind, Kind.List)
  assertEquals(result.array, [-5, 0, 1.8, 1.2, 1])
})

Deno.test('math/naivemethic: evaluates to 60', async () => {
  const result: number = await LoreTest.run('math/naivemethic')
  assertEquals(result, 60)
})
