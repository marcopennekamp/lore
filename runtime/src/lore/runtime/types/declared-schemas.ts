import { TraitType } from '../traits.ts'
import { TupleType } from '../tuples.ts'
import { TypeMap, TypeMaps } from '../utils/TypeMap.ts'
import { DeclaredType } from './declared-types.ts'
import { TypeVariable } from './type-variables.ts'

export interface DeclaredSchema {
  name: string
  typeParameters: Array<TypeVariable>

  /**
   * A list of directly extended traits, potentially containing uninstantiated type parameters.
   */
  supertraits: Array<TraitType>

  /**
   * Whether the declared type inherits multiple times from the same supertype with differing type arguments. This is
   * determined at compile time.
   */
  hasMultipleParameterizedInheritance: boolean

  /**
   * The representative type of this schema, without instantiated type parameters.
   */
  representative: DeclaredType

  /**
   * The type cache contains all interned declared types that have this schema. Only defined for parametric schemas.
   */
  typeCache?: TypeMap<TupleType, DeclaredType>
}

export const DeclaredSchemas = {
  /**
   * Creates a declared schema from the given arguments.
   */
  schema<S extends DeclaredSchema>(
    name: string,
    typeParameters: Array<TypeVariable>,
    supertraits: Array<TraitType>,
    hasMultipleParameterizedInheritance: boolean,
    getRepresentative: (schema: S, typeArguments: Array<TypeVariable> | undefined) => DeclaredType,
    extras: object,
  ): S {
    const schema = { name, typeParameters, supertraits, hasMultipleParameterizedInheritance, representative: undefined, ...extras } as unknown as S
    schema.representative = getRepresentative(schema, typeParameters.length > 0 ? typeParameters : undefined)
    if (typeParameters.length > 0) {
      schema.typeCache = TypeMaps.create()
    }
    return schema
  },

  /**
   * The number of expected type arguments.
   */
  arity(schema: DeclaredSchema): number {
    return schema.typeParameters.length
  },

  /**
   * A constant schema has no type parameters (arity 0) and is thus effectively equal to a single declared type.
   */
  isConstant(schema: DeclaredSchema): boolean {
    return DeclaredSchemas.arity(schema) === 0
  },
}
