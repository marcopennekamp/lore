import { assert } from 'https://deno.land/std/testing/asserts.ts'
import { areEqual } from '../../../../src/lore/runtime/types/equality.ts'
import { boolean, int, intersection, intersectionSimplified, real, shape, string } from '../../../../src/lore/runtime/types/types.ts'

Deno.test("types/intersection: intersection and shape types are correctly simplified", () => {
  assert(areEqual(
    intersectionSimplified([
      shape({ x: real, z: real }),
      shape({ x: int, y: int }),
      shape({ name: string }),
      shape({ name: intersectionSimplified([string, boolean]) })
    ]),
    shape({ x: int, y: int, z: real, name: intersection([string, boolean]) })
  ))
})
