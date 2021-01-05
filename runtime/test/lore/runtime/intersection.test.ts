import { assert } from 'https://deno.land/std/testing/asserts.ts'
import { Intersection } from '../../../src/lore/runtime/intersections.ts'
import { Shape } from '../../../src/lore/runtime/shapes.ts'
import { areEqual } from '../../../src/lore/runtime/types/equality.ts'
import { Types } from '../../../src/lore/runtime/types/types.ts'

Deno.test("intersection: intersection and shape types are correctly simplified", () => {
  assert(areEqual(
    Intersection.simplified([
      Shape.type({ x: Types.real, z: Types.real }),
      Shape.type({ x: Types.int, y: Types.int }),
      Shape.type({ name: Types.string }),
      Shape.type({ name: Intersection.simplified([Types.string, Types.boolean]) })
    ]),
    Shape.type({ x: Types.int, y: Types.int, z: Types.real, name: Intersection.type([Types.string, Types.boolean]) })
  ))
})
