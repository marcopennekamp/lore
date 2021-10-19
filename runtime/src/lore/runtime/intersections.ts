import { Simplification } from './simplification.ts'
import { BasicType } from './types/basic-types.ts'
import { Kind } from './types/kinds.ts'
import { Type, XaryType } from './types/types.ts'
import { unorderedHashWithSeed } from './utils/hash.ts'

export interface IntersectionType extends XaryType { }

export const Intersection = {
  type(types: Array<Type>): IntersectionType {
    return { kind: Kind.Intersection, types, hash: unorderedHashWithSeed(types, 0x74a2317d) }
  },

  /**
   * Simplifies the given intersection type parts according to our intersection type normal form. If the resulting
   * intersection type would only contain a single part, we instead return the type itself.
   */
  simplified(parts: Array<Type>): Type {
    if (parts.length === 0) {
      return BasicType.any
    }
    return Simplification.construct(Kind.Intersection, parts)
  },
}
