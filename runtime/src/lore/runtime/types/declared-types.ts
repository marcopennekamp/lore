import { Intersection } from '../intersections.ts'
import { Sum } from '../sums.ts'
import { Trait, TraitSchema, TraitType } from '../traits.ts'
import { orderedHash, pairHashRaw, stringHash, stringHashWithSeed } from '../utils/hash.ts'
import { DeclaredSchemas, DeclaredTypeSchema } from './declared-schemas.ts'
import { Kind } from './kinds.ts'
import { Assignments, Variance } from './type-variables.ts'
import { Type } from './types.ts'
import { unique } from './util.ts'

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

  /**
   * Finds the supertrait with the given schema that `type` inherits from, or `undefined` if `type` inherits from no
   * such supertype schema. The algorithm combines all occurrences of the trait if `type` inherits from it multiple
   * times.
   *
   * TODO (schemas): The current implementation essentially scans the whole supertrait hierarchy. Is there any way in
   *                 which we could improve performance? Probably at the sacrifice of some memory.
   *                    - Idea 1: Keep a flat hash map of Schema -> Type in d1's schema, with uninstantiated type
   *                              arguments. It contains a transitive closure of all supertypes. We can then quickly get
   *                              the correct type with the given schema, and instantiate it as i1, then check if i1 is
   *                              a subtype of t2. (The advantage is further that we probably don't need the more
   *                              complicated algorithm for when `hasMultipleParameterizedInheritance` is true. We can
   *                              combine these types at compile time when the Schema -> Type map is generated.)
   *                                - The reverse (having a subtype map in the supertrait) would not work because there
   *                                  is no straight-forward way to handle type arguments directly.
   *                    - Idea 2: Idea 1, but build the hash map slowly as a cache. This would require us to implement
   *                              all relevant algorithms (including the one for `hasMultipleParameterizedInheritance`),
   *                              but might save memory since not all subtype/supertype combinations will likely be
   *                              checked. For example, it is unlikely, albeit possible, that d1 is even a trait. So most
   *                              of the caching will happen in structs. If we have a struct Fox <: (Mammal <: (Animal
   *                              <: Hashable)) but we never check Fox <: Mammal and neither Fox <: Hashable, the cache
   *                              of Fox will only have one entry `Animal<schema> -> Animal<representative>`.
   *                 The big downside here is memory. Suppose we have a type hierarchy where T1 has 10 map entries and T2
   *                 has 12 map entries. A type T3 that extends both T1 and T2 will have 10 + 12 + 2 = 24 map entries,
   *                 unless T1 and T2 share common supertraits.
   *                 Note that such a caching mechanism will require us to traverse the SCHEMA'S supertrait hierarchy
   *                 and instantiate type parameters as the last step. This might even render DeclaredType.supertraits
   *                 obsolete, which would save a lot of memory. (Well, the first layer would be to get `type.schema.supertraits`,
   *                 but from that point on, we need each trait TYPE's supertraits, so this would require substituting.
   *                 However, for each subtype/supertype combination, this substitution would only happen once and only
   *                 the result would be saved in the transitive supertype cache.)
   */
  findSupertrait(type: DeclaredType, supertypeSchema: TraitSchema): TraitType | undefined {
    if (!type.schema.hasMultipleParameterizedInheritance || DeclaredSchemas.isConstant(supertypeSchema)) {
      return getFirstSupertrait(type, supertypeSchema)
    } else {
      return getCombinedSupertrait(type, supertypeSchema)
    }
  },
}

/**
 * Finds the first occurrence of schema2 in d1's supertraits. This operation is only applicable if the d1's schema's
 * `hasMultipleParameterizedInheritance` flag is set to `false` or if schema2 is constant.
 */
function getFirstSupertrait(dt: DeclaredType, supertypeSchema: TraitSchema): TraitType | undefined {
  const queue = dt.supertraits.slice()
  while (queue.length > 0) {
    const candidate = <TraitType> queue.pop()
    if (candidate.schema === supertypeSchema) {
      return candidate
    }
    queue.push(...candidate.supertraits)
  }
  return undefined
}

/**
 * Finds all occurrences of schema2 in d1's supertraits. This function is only used when schema2 has type parameters.
 */
function getCombinedSupertrait(dt: DeclaredType, supertypeSchema: TraitSchema): TraitType | undefined {
  const candidates: Array<TraitType> = []
  const collect = (type: DeclaredType) => {
    if (type.schema === supertypeSchema) {
      candidates.push(<TraitType> type)
    } else {
      for (const supertype of type.supertraits) {
        collect(supertype)
      }
    }
  }
  collect(dt)

  if (candidates.length <= 1) {
    return candidates[0]
  }

  const parameters = supertypeSchema.typeParameters
  const combinedArguments = new Array(parameters.length)
  for (let i = 0; i < parameters.length; i += 1) {
    const parameter = parameters[i]
    // @ts-ignore
    const argumentCandidates = candidates.map(c => c.typeArguments[i])
    switch (parameter.variance) {
      case Variance.Covariant:
        combinedArguments[i] = Intersection.simplified(argumentCandidates)
        break

      case Variance.Contravariant:
        combinedArguments[i] = Sum.simplified(argumentCandidates)
        break

      case Variance.Invariant: {
        const uniqueArguments = unique(argumentCandidates)
        if (uniqueArguments.length == 1) {
          combinedArguments[i] = uniqueArguments[0]
        } else {
          return undefined
        }
        break
      }
    }
  }

  return Trait.type(supertypeSchema, combinedArguments)
}
