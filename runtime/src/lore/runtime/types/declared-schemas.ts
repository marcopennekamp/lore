import { TraitType } from '../traits.ts'
import { DeclaredType } from './declared-types.ts'
import { substitute } from './substitution.ts'
import { Assignments, TypeVariable, TypeVariables, Variance } from './type-variables.ts'

// TODO (schemas): Rename to DeclaredSchema.
export interface DeclaredTypeSchema {
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
}

export const DeclaredSchemas = {
  /**
   * Creates a declared schema from the given arguments.
   *
   * TODO (schemas): Also initialize a type map (interning, caching) for parameterized schemas?
   */
  schema<S extends DeclaredTypeSchema>(
    name: string,
    typeParameters: Array<TypeVariable>,
    supertraits: Array<TraitType>,
    hasMultipleParameterizedInheritance: boolean,
    getRepresentative: (schema: S, typeArguments: Array<TypeVariable> | undefined) => DeclaredType,
    extras: object,
  ): S {
    const schema = { name, typeParameters, supertraits, hasMultipleParameterizedInheritance, representative: undefined, ...extras } as unknown as S
    schema.representative = getRepresentative(schema, typeParameters.length > 0 ? typeParameters : undefined)
    return schema
  },

  /**
   * The number of expected type arguments.
   */
  arity(schema: DeclaredTypeSchema): number {
    return schema.typeParameters.length
  },

  /**
   * A constant schema has no type parameters (arity 0) and is thus effectively equal to a single declared type.
   */
  isConstant(schema: DeclaredTypeSchema): boolean {
    return DeclaredSchemas.arity(schema) === 0
  },

  /**
   * Checks whether the given type arguments fit into the schema's parameter bounds. Upper bounds for covariance and
   * lower bounds for contravariance are guaranteed by the compiler, but we need to check lower/upper bounds for
   * covariant/contravariant type parameters.
   *
   * We don't check arity for performance reasons. The compiler should always transpile an instantiation with the
   * correct number of type arguments.
   */
  boundsContain(schema: DeclaredTypeSchema, typeArguments: Assignments): boolean {
    const parameters = schema.typeParameters
    for (let i = 0; i < parameters.length; i += 1) {
      const parameter = parameters[i]
      const argument = typeArguments[i]
      if (parameter.variance === Variance.Covariant) {
        if (!TypeVariables.lowerBoundContains(parameter, argument, typeArguments)) return false
      } else if (parameter.variance === Variance.Contravariant) {
        if (!TypeVariables.upperBoundContains(parameter, argument, typeArguments)) return false
      }
    }
    return true
  },

  /**
   * Instantiates the schema's supertraits with the given type arguments. If the schema is constant, no substitutions
   * are required.
   */
  instantiateSupertraits(schema: DeclaredTypeSchema, typeArguments: Assignments): Array<TraitType> {
    const supertraits = schema.supertraits
    if (DeclaredSchemas.isConstant(schema) || supertraits.length === 0) {
      return supertraits
    }

    const result = new Array(supertraits.length)
    for (let i = 0; i < supertraits.length; i += 1) {
      result[i] = substitute(typeArguments, supertraits[i])
    }
    return result
  },
}
