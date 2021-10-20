// TODO: Write subtyping tests.
//       The problem is that we should test both the runtime and the compiler with the same types to ensure
//       feature parity. This is paramount since we have two parallel implementations. Instead of replicating
//       the tests, which is error-prone itself, we should try to define these tests in some known file format
//       and then interpret these "config" files in each of our test suites.

import { assert } from 'https://deno.land/std/testing/asserts.ts'
import { Sum } from '../../../../src/lore/runtime/sums.ts'
import { isSubtype } from '../../../../src/lore/runtime/types/subtyping.ts'
import { Types } from '../../../../src/lore/runtime/types/types.ts'

Deno.test("types/subtyping: sum types are subtyped correctly", () => {
  assert(isSubtype(
    Sum.simplified([Sum.type([Types.string, Types.number]), Types.boolean]),
    Sum.type([Types.string, Types.number, Types.boolean])
  ))
})
