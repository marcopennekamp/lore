import { Struct, StructSchema, StructType } from '../structs.ts'
import { Trait, TraitSchema, TraitType } from '../traits.ts'
import { TupleType } from '../tuples.ts'
import { TypeMap, TypeMaps } from '../utils/TypeMap.ts'
import { DeclaredType } from './declared-types.ts'
import { Kind } from './kinds.ts'
import { TypeVariable, Variance } from './type-variables.ts'
import { Type } from './types.ts'

export interface DeclaredSchema {
  kind: Kind.Trait | Kind.Struct
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
   * Whether the schema has invariant type parameters.
   */
  hasInvariantParameters: boolean

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
    kind: Kind.Trait | Kind.Struct,
    name: string,
    typeParameters: Array<TypeVariable>,
    supertraits: Array<TraitType>,
    hasMultipleParameterizedInheritance: boolean,
    getRepresentative: (schema: S, typeArguments: Array<TypeVariable> | undefined) => DeclaredType,
    extras: object,
  ): S {
    const schema = { kind, name, typeParameters, supertraits, hasMultipleParameterizedInheritance, representative: undefined, ...extras } as unknown as S
    schema.hasInvariantParameters = typeParameters.some(tv => tv.variance === Variance.Invariant)
    schema.representative = getRepresentative(schema, typeParameters.length > 0 ? typeParameters : undefined)
    if (typeParameters.length > 0) {
      schema.typeCache = TypeMaps.create()
    }
    return schema
  },

  /**
   * Instantiates a declared type of the given schema with the given type arguments. The resulting struct type's open
   * property types will be empty.
   *
   * This function can be used instead of `Trait.type` or `Struct.type` if the concrete kind of the schema is unknown
   * at compile time.
   */
  type(schema: DeclaredSchema, typeArguments: Array<Type>): DeclaredType {
    if (schema.kind === Kind.Trait) {
      return Trait.type(<TraitSchema> schema, typeArguments)
    } else {
      return Struct.type(<StructSchema> schema, typeArguments)
    }
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
