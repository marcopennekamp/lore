import { ProductType, Type, TypeVariable } from './types.ts'
import { isPolymorphic } from './polymorphy.ts'
import { areEqual } from './equality.ts'
import { isSubtype } from './subtyping.ts'
import { Kind } from './kinds.ts'
import { ListType, MapType} from './types.ts'
import { substitute } from './substitution.ts'

export type Assignments = Map<TypeVariable, Type>

/**
 * Whether t1 fits into t2.
 */
export function fits(t1: Type, t2: Type): boolean {
  if (t1 === t2) return true

  let assignments
  if (isPolymorphic(t2)) {
    const allocation = TypeVariableAllocation.of(t1, t2)
    if (!allocation.isConsistent()) {
      return false
    }
    assignments = allocation.assignments()
  } else {
    assignments = new Map()
  }

  // TODO: Check missing variables? (See the compiler's corresponding fit definition.)

  const st2 = substitute(assignments, t2)
  return isSubtype(t1, st2)
}

class TypeVariableAllocation {
  /**
   * Note: We have to be careful here. JS maps use object references as the hash key. This is fine in the case of
   * type variables, but wouldn't be fine in other cases. Watch out if this ever changes.
   */
  private allocation: Map<TypeVariable, Array<Type>> = new Map()

  addAssignment(tv: TypeVariable, tpe: Type): void {
    let assignments = this.allocation.get(tv)
    if (!assignments) {
      assignments = []
      this.allocation.set(tv, assignments)
    }
    assignments.push(tpe)
  }

  assignments(): Assignments {
    const assignments = new Map()
    for (let entry of this.allocation.entries()) {
      assignments.set(entry[0], entry[1][0])
    }
    return assignments
  }

  isConsistent(): boolean {
    for (let entry of this.allocation.entries()) {
      const possibleAssignments = entry[1]
      for (let i = 0; i < possibleAssignments.length - 1; i += 1) {
        const left = possibleAssignments[i]
        const right = possibleAssignments[i + 1]
        if (!areEqual(left, right)) return false
      }
    }

    const assignments = this.assignments()
    for (let entry of assignments.entries()) {
      const variable = entry[0]
      const type = entry[1]
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
      case Kind.Class:
      case Kind.Label:
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
      case Kind.Component:
        break // TODO: Change this once we allow type parameters for classes and labels?
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
