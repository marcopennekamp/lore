import { Shape, ShapeType } from './shapes.ts'
import { BasicType } from './types/basic-types.ts'
import { Kind } from './types/kinds.ts'
import { isSubtype } from './types/subtyping.ts'
import { Type, XaryType } from './types/types.ts'
import { allExcluding, flattenedUnique } from './types/util.ts'
import { unorderedHashWithSeed } from './utils/hash.ts'

export interface IntersectionType extends XaryType { }

export const Intersection = {
  type(types: Array<Type>): IntersectionType {
    return { kind: Kind.Intersection, types, hash: unorderedHashWithSeed(types, 0x74a2317d) }
  },

  /**
   * Simplifies the given intersection type parts according to our intersection type normal form. If the resulting
   * intersection type would only contain a single part, we instead return the type itself.
   *
   * TODO (inference): Implement the same type of simplification as present at compile time.
   */
  simplified(types: Array<Type>): Type {
    if (types.length === 0) {
      return BasicType.any
    }

    if (types.length === 1) {
      return types[0]
    }

    // TODO: If we ordered types by some type ordering, we could probably achieve a speedup for subtyping, equality
    //       checking, and also simplification that involves intersection and/or sum types.
    const flattened = flattenedUnique(Kind.Intersection, types)

    const noShapes: Array<Type> = []
    const shapes: Array<ShapeType> = []
    for (const type of types) {
      if (type.kind === Kind.Shape) {
        shapes.push(<ShapeType> type)
      } else {
        noShapes.push(type)
      }
    }

    let shapesCombined = flattened
    if (shapes.length > 1) {
      noShapes.push(Shape.combine(shapes))
      shapesCombined = noShapes
    }

    // Remove strict supertypes of other parts. Conceptually, we have to check for strict supertypes here, but we have
    // already established that no two types in the list are equal. Since the definition of strict supertyping t1 > t2
    // is t1 != t2 && t1 >= t2, we can forego the strict check here and just use normal subtyping.
    const simplified = allExcluding(shapesCombined, (self, other) => isSubtype(other, self))

    if (simplified.length === 1) {
      return simplified[0]
    } else {
      return Intersection.type(simplified)
    }
  },
}
