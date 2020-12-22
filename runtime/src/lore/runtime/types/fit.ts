import { ListType, MapType, ProductType, Type, TypeVariable } from './types.ts'
import { isPolymorphic } from './polymorphy.ts'
import { areEqual } from './equality.ts'
import { isSubtype } from './subtyping.ts'
import { Kind } from './kinds.ts'
import { substitute } from './substitution.ts'
import { TinyMap } from '../utils/TinyMap.ts'

export type Assignments = TinyMap<TypeVariable, Type>

/**
 * Whether t1 fits into t2.
 *
 * This function is used by API calls which don't know whether t2 is polymorphic or not. The compiler does not
 * call this function directly.
 */
export function fits(t1: Type, t2: Type): boolean {
  if (isPolymorphic(t2)) {
    return !!fitsPolymorphic(t1, t2)
  }
  return fitsMonomorphic(t1, t2)
}

/**
 * Whether t1 fits into (monomorphic) t2.
 */
export function fitsMonomorphic(t1: Type, t2: Type): boolean {
  return isSubtype(t1, t2)
}

/**
 * Returns a set of type variable assignments if t1 fits into (polymorphic) t2. Otherwise returns false to
 * signal that t1 does not fit into t2. The assignments map is used during multiple dispatch to pass type context
 * to the called polymorphic function, which can then be used to construct new types such as lists, maps, and later
 * type-parametric declared types.
 */
export function fitsPolymorphic(t1: Type, t2: Type): Assignments | boolean {
  const allocation = TypeVariableAllocation.of(t1, t2)
  if (!allocation.isConsistent()) {
    return false
  }

  const assignments = allocation.assignments()
  // TODO: Check missing variables? (See the compiler's corresponding fit definition.)
  const st2 = substitute(assignments, t2)
  if (!isSubtype(t1, st2)) {
    return false
  }

  return assignments
}

class TypeVariableAllocation {
  /**
   * Note: We have to be careful here. JS maps use object references as the hash key. This is fine in the case of
   * type variables, but wouldn't be fine in other cases. Watch out if this ever changes.
   */
  private allocation: TinyMap<TypeVariable, Array<Type>> = []

  addAssignment(tv: TypeVariable, tpe: Type): void {
    const allocation = this.allocation
    let assignments = TinyMap.get(allocation, tv)
    if (!assignments) {
      assignments = []
      TinyMap.set(allocation, tv, assignments)
    }
    assignments.push(tpe)
  }

  private assignments_: Assignments | undefined

  assignments(): Assignments {
    if (this.assignments_) return this.assignments_
    const allocation = this.allocation
    this.assignments_ = []
    const assignments = this.assignments_
    for (let i = 0; i < allocation.length; i += 1) {
      const entry = allocation[i]
      TinyMap.set(assignments, entry.key, entry.value[0])
    }
    return assignments
  }

  isConsistent(): boolean {
    const allocation = this.allocation
    for (let i = 0; i < allocation.length; i += 1) {
      const entry = allocation[i]
      const possibleAssignments = entry.value
      for (let i = 0; i < possibleAssignments.length - 1; i += 1) {
        const left = possibleAssignments[i]
        const right = possibleAssignments[i + 1]
        if (!areEqual(left, right)) return false
      }
    }

    const assignments = this.assignments()
    for (let i = 0; i < assignments.length; i += 1) {
      const entry = assignments[i]
      const variable = entry.key
      const type = entry.value
      const actualLowerBound = substitute(assignments, variable.lowerBound)
      const actualUpperBound = substitute(assignments, variable.upperBound)
      if (!isSubtype(actualLowerBound, type) || !isSubtype(type, actualUpperBound)) {
        return false
      }
    }
    return true
  }

  /**
   * Creates a type variable allocation that represents all type variables in t2 which have been assigned types
   * from t1.
   */
  static of(t1: Type, t2: Type): TypeVariableAllocation {
    const allocation = new TypeVariableAllocation()
    TypeVariableAllocation.assign(t1, t2, allocation)
    return allocation
  }

  private static assign(t1: Type, t2: Type, allocation: TypeVariableAllocation): void {
    if (t2.kind === Kind.TypeVariable) {
      allocation.addAssignment(<TypeVariable> t2, t1)
      return
    }

    if (t1.kind === Kind.Intersection || t2.kind === Kind.Intersection || t1.kind === Kind.Sum || t2.kind === Kind.Sum) {
      throw Error("Intersection and sum type type variable allocations are not yet supported.")
    }

    switch (t2.kind) {
      case Kind.Struct:
      case Kind.Trait:
        break // TODO: Change this once we allow type parameters for classes and labels.

      case Kind.Product:
        if (t1.kind === Kind.Product) {
          const types1 = (<ProductType> t1).types
          const types2 = (<ProductType> t2).types
          if (types1.length === types2.length) {
            for (let i = 0; i < types1.length; i += 1) {
              TypeVariableAllocation.assign(types1[i], types2[i], allocation)
            }
          }
        }
        break

      case Kind.List:
        if (t1.kind === Kind.List) {
          const l1 = <ListType> t1
          const l2 = <ListType> t2
          TypeVariableAllocation.assign(l1.element, l2.element, allocation)
        }
        break

      case Kind.Map:
        if (t1.kind === Kind.Map) {
          const m1 = <MapType> t1
          const m2 = <MapType> t2
          TypeVariableAllocation.assign(m1.key, m2.key, allocation)
          TypeVariableAllocation.assign(m1.value, m2.value, allocation)
        }
        break
    }
  }
}
