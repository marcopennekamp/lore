import { assert } from 'https://deno.land/std/testing/asserts.ts'
import { Tuple } from '../../../../src/lore/runtime/tuples.ts'
import { areEqual } from '../../../../src/lore/runtime/types/equality.ts'

// TODO: Write equality tests.

Deno.test("types/equality: unit types are equal", () => {
  assert(areEqual(Tuple.type([]), Tuple.unitType))
})
