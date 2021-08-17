import { Kind } from './kinds.ts'
import { substitute } from './substitution.ts'
import { isSubtype } from './subtyping.ts'
import { Type } from './types.ts'

/**
 * A run-time type variable is identified by its index in the type argument array of the schema or function call.
 */
export interface TypeVariable extends Type {
  index: number
  lowerBound: Type
  upperBound: Type
  variance: Variance

  /**
   * The variable's full name is used when it is stringified and to generate its hash code. The full name is composed
   * of an owner name such as a function name or a class name, and the variable's name itself.
   *
   * The point of a *full* name as opposed to a simple name is the need to generate unique hash codes for unique type
   * variables. A lot of type variables across many contexts may be named "A" (and have index 0). Qualifying that name
   * with an owner prefix (e.g. `create.A`, `Container.A`, `find.A`) produces better hash codes.
   */
  fullName: string
}

export enum Variance {
  Covariant,
  Contravariant,
  Invariant,
}

/**
 * Type variable assignments accessed by type variable indices. The order and number of elements is always implicitly
 * defined by some type parameter list.
 *
 * Run-time assignments are an array instead of a map for improved performance. A `TypeVariable --> Type` map would be
 * clearer, but an array is equally usable here and much faster.
 */
export type Assignments = Array<Type>

export const TypeVariables = {
  boundsContain(variable: TypeVariable, type: Type, assignments: Assignments): Boolean {
    return TypeVariables.upperBoundContains(variable, type, assignments) &&
      TypeVariables.lowerBoundContains(variable, type, assignments)
  },

  /**
   * Whether the given variable's upper bound (instantiated via the given assignments) contains `type`.
   *
   * TODO: Performance improvement: Only substitute assignments if the variable bounds are polymorphic. This flag could
   *       be prepared at compile time.
   */
  upperBoundContains(variable: TypeVariable, type: Type, assignments: Assignments): Boolean {
    if (variable.upperBound.kind !== Kind.Any) {
      const actualUpperBound = substitute(assignments, variable.upperBound)
      if (!isSubtype(type, actualUpperBound)) {
        return false
      }
    }
    return true
  },

  /**
   * Whether the given variable's lower bound (instantiated via the given assignments) contains `type`.
   */
  lowerBoundContains(variable: TypeVariable, type: Type, assignments: Assignments): Boolean {
    if (variable.lowerBound.kind !== Kind.Nothing) {
      const actualLowerBound = substitute(assignments, variable.lowerBound)
      if (!isSubtype(actualLowerBound, type)) {
        return false
      }
    }
    return true
  },
}
