import { Kind } from './types/kinds.ts'
import { DeclaredType, DeclaredTypeSchema } from './types/types.ts'
import { stringHashWithSeed } from './utils/hash.ts'

export interface TraitSchema extends DeclaredTypeSchema { }

/**
 * A trait type. Only one type is instantiated for each trait, as their supertypes are not dependent on run-time
 * values. Thus, a trait type is also always an archetype.
 */
export interface TraitType extends DeclaredType {
  schema: TraitSchema
}

export const Trait = {
  schema(name: string, supertraits: Array<TraitType>): TraitSchema {
    return { name, supertraits }
  },

  type(schema: TraitSchema): TraitType {
    return { kind: Kind.Trait, schema, isArchetype: true, hash: stringHashWithSeed(schema.name, 0x38ba128e) }
  },
}
