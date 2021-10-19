import { Simplification } from './simplification.ts'
import { BasicType } from './types/basic-types.ts'
import { Kind } from './types/kinds.ts'
import { Type, XaryType } from './types/types.ts'
import { unorderedHashWithSeed } from './utils/hash.ts'

// TODO: If we ordered types by some type ordering, we could probably achieve a speedup for subtyping, equality
//       checking, and also simplification that involves intersection and/or sum types.

export interface SumType extends XaryType { }

export const Sum = {
  type(types: Array<Type>): SumType {
    return { kind: Kind.Sum, types, hash: unorderedHashWithSeed(types, 0x85f5fe35) }
  },

  /**
   * Simplifies the given sum type parts according to our sum type normal form. If the resulting sum type would only
   * contain a single part, we instead return the type itself.
   */
  simplified(parts: Array<Type>): Type {
    if (parts.length === 0) {
      return BasicType.nothing
    }
    return Simplification.construct(Kind.Sum, parts)
  },
}
