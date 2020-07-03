import { assert } from 'https://deno.land/std/testing/asserts.ts'
import { areEqual } from '../../../../src/lore/runtime/types/equality.ts'
import { product, unit } from '../../../../src/lore/runtime/types/types.ts'

// TODO: Write equality tests.

Deno.test("types/equality: unit types are equal", () => {
  assert(areEqual(product([]), unit));
});
