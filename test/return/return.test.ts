import { LoreTest } from '../base.ts'
import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { TupleValue } from '../../runtime/src/lore/runtime/values/tuple.ts'
import { Kind } from '../../runtime/src/lore/runtime/types/kinds.ts'

Deno.test("return/simple: evaluates to (10, 1, 5)", async () => {
  await LoreTest.compile('return/simple')
  const result: TupleValue = await LoreTest.execute()
  assertEquals(result.lore$type.kind, Kind.Product)
  assertEquals(result.elements, [10, 1, 5])
});
