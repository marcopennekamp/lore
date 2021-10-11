import { BasicType } from './types/basic-types.ts'
import { Kind } from './types/kinds.ts'
import { isSubtype } from './types/subtyping.ts'
import { Type, XaryType } from './types/types.ts'
import { allExcluding, flattenedUnique } from './types/util.ts'
import { unorderedHashWithSeed } from './utils/hash.ts'

export interface SumType extends XaryType { }

export const Sum = {
  type(types: Array<Type>): SumType {
    return { kind: Kind.Sum, types, hash: unorderedHashWithSeed(types, 0x85f5fe35) }
  },

  /**
   * Simplifies the given sum type parts according to our sum type normal form. If the resulting sum type would only
   * contain a single part, we instead return the type itself.
   *
   * TODO (inference): Implement the same type of simplification as present at compile time.
   */
  simplified(types: Array<Type>): Type {
    if (types.length === 0) {
      return BasicType.nothing
    }

    if (types.length === 1) {
      return types[0]
    }

    // TODO: If we ordered types by some type ordering, we could probably achieve a speedup for subtyping, equality
    //       checking, and also simplification that involves intersection and/or sum types.
    const flattened = flattenedUnique(Kind.Sum, types)

    // Remove strict subtypes of other parts. Conceptually, we have to check for strict subtypes here, but we have
    // already established that no two types in the list are equal. Since the definition of strict subtyping t1 < t2
    // is t1 != t2 && t1 <= t2, we can forego the strict check here and just use normal subtyping.
    const simplified = allExcluding(flattened, (self, other) => isSubtype(self, other))

    if (simplified.length === 1) {
      return simplified[0]
    } else {
      return Sum.type(simplified)
    }
  },
}
