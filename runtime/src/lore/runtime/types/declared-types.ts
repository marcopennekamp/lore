import { TraitType } from '../traits.ts'
import { orderedHash, pairHashRaw, stringHash, stringHashWithSeed } from '../utils/hash.ts'
import { DeclaredSchemas, DeclaredTypeSchema } from './declared-schemas.ts'
import { Kind } from './kinds.ts'
import { Assignments} from './type-variables.ts'
import { Type } from './types.ts'

export interface DeclaredType extends Type {
  schema: DeclaredTypeSchema

  /**
   * The declared type's type argument. This field must and may only be `undefined` if the schema is constant.
   */
  typeArguments?: Assignments

  /**
   * The schema's supertraits, instantiated with the given type arguments.
   */
  supertraits: Array<TraitType>
}

export const DeclaredTypes = {
  /**
   * Creates a declared type from the given arguments.
   *
   * `typeArguments` must and may only be `undefined` if the schema is constant.
   *
   * TODO (schemas): Interning: Get an existing declared type if it exists in the type cache.
   */
  type<T extends DeclaredType>(kind: Kind.Trait | Kind.Struct, schema: DeclaredTypeSchema, typeArguments: Assignments | undefined, extras: object, hash: number): T {
    let supertraits
    if (typeArguments) {
      if (!DeclaredSchemas.boundsContain(schema, typeArguments)) {
        throw Error(`Cannot instantiate schema ${schema} with type arguments ${typeArguments}.`)
      }
      supertraits = DeclaredSchemas.instantiateSupertraits(schema, typeArguments)
    } else {
      supertraits = schema.supertraits
    }

    return { kind, schema, typeArguments, supertraits, ...extras, hash } as unknown as T
  },

  hash(schema: DeclaredTypeSchema, typeArguments: Assignments | undefined): number {
    if (!typeArguments) {
      return stringHashWithSeed(schema.name, 0x38ba128e)
    }
    return pairHashRaw(stringHash(schema.name), orderedHash(typeArguments), 0x38ba128e)
  },
}
