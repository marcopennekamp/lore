import { FunctionType } from '../functions.ts'
import { ListType } from '../lists.ts'
import { MapType } from '../maps.ts'
import { ShapeType } from '../shapes.ts'
import { Struct, StructType } from '../structs.ts'
import { TraitSchema } from '../traits.ts'
import { TupleType } from '../tuples.ts'
import { TinyMap } from '../utils/TinyMap.ts'
import { DeclaredType, DeclaredTypes } from './declared-types.ts'
import { areEqual } from './equality.ts'
import { Kind } from './kinds.ts'
import { isPolymorphic, variables } from './polymorphy.ts'
import { substitute } from './substitution.ts'
import { isSubtype } from './subtyping.ts'
import { Assignments, TypeVariable, TypeVariables } from './type-variables.ts'
import { Type } from './types.ts'

/**
 * Whether t1 fits into t2.
 *
 * This function is used by API calls which don't know whether t2 is polymorphic or not. The compiler does not
 * call this function directly.
 */
export function fits(t1: Type, t2: Type): boolean {
  if (isPolymorphic(t2)) {
    return !!fitsPolymorphic(t1, t2, variables(t2))
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
 * signal that t1 does not fit into t2. The assignments are used during multiple dispatch to pass type context to the
 * called polymorphic function, which can then be used to construct new types such as lists, maps, and parametric
 * declared types.
 *
 * @param variables A transpiled list of variables contained in `t2`. This list is precompiled so that we don't have to
 *                  calculate it at run time.
 */
export function fitsPolymorphic(t1: Type, t2: Type, variables: Array<TypeVariable>): Assignments | false {
  const allocation = TypeVariableAllocation.of(t1, t2)
  if (!allocation.isConsistent(variables)) {
    return false
  }

  const assignments = allocation.assignments()
  const st2 = substitute(assignments, t2)
  if (!isSubtype(t1, st2)) {
    return false
  }

  return assignments
}

class TypeVariableAllocation {
  /**
   * Note: We have to be careful here. JS maps use object references as the hash key. This is fine in the case of type
   * variables, but wouldn't be fine in other cases.
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
      assignments[entry.key.index] = entry.value[0]
    }
    return assignments
  }

  /**
   * Whether the type variable allocation is consistent in respect to the given expected variables.
   */
  isConsistent(variables: Array<TypeVariable>): boolean {
    return this.allVariablesAssigned(variables) && this.areAssignmentsUnique() && this.areBoundsKept(variables)
  }

  private allVariablesAssigned(variables: Array<TypeVariable>): boolean {
    const allocation = this.allocation
    for (const variable of variables) {
      if (!TinyMap.get(allocation, variable)) return false
    }
    return true
  }

  private areAssignmentsUnique(): boolean {
    const allocation = this.allocation
    for (let i = 0; i < allocation.length; i += 1) {
      const entry = allocation[i]
      const possibleAssignments = entry.value
      for (let j = 0; j < possibleAssignments.length - 1; j += 1) {
        const left = possibleAssignments[j]
        const right = possibleAssignments[j + 1]
        if (!areEqual(left, right)) return false
      }
    }
    return true
  }

  private areBoundsKept(variables: Array<TypeVariable>): boolean {
    const assignments = this.assignments()

    for (const variable of variables) {
      const type = assignments[variable.index]
      if (!TypeVariables.boundsContain(variable, type, assignments)) {
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

  // TODO: Optimization idea: Map type variables to indices at compile time so that we don't need a map here at run
  //       time. Also assign types to type variables in such a way that we test for equality right away. There is no
  //       need to defer the equality check to later. This also means we can fail faster.
  private static assign(t1: Type, t2: Type, allocation: TypeVariableAllocation): void {
    switch (t2.kind) {
      case Kind.TypeVariable:
        allocation.addAssignment(<TypeVariable> t2, t1)
        return

      case Kind.Trait:
      case Kind.Struct:
        if (t1.kind === Kind.Trait || t1.kind === Kind.Struct) {
          const d1 = <DeclaredType> t1
          const d2 = <DeclaredType> t2

          // We can only assign types from type arguments if the declared type is parametric.
          if (d2.typeArguments) {
            let s1: DeclaredType | undefined
            if (d2.kind === Kind.Trait) {
              s1 = DeclaredTypes.findSupertrait(d1, <TraitSchema> d2.schema)
            } else if (d1.schema === d2.schema) {
              s1 = d1
            }

            if (s1) {
              for (let i = 0; i < d2.typeArguments.length; i += 1) {
                // @ts-ignore
                const a1 = s1.typeArguments[i]
                const a2 = d2.typeArguments[i]
                this.assign(a1, a2, allocation)
              }
            }
          }
        }
        break

      case Kind.Sum:
        if (isPolymorphic(t2)) {
          throw Error('Type variable allocations for sum types are not yet supported.')
        }
        break

      case Kind.Intersection:
        if (isPolymorphic(t2)) {
          throw Error('Type variable allocations for intersection types are not yet supported.')
        }
        break

      case Kind.Tuple:
        if (t1.kind === Kind.Tuple) {
          const types1 = (<TupleType> t1).types
          const types2 = (<TupleType> t2).types
          if (types1.length === types2.length) {
            for (let i = 0; i < types1.length; i += 1) {
              TypeVariableAllocation.assign(types1[i], types2[i], allocation)
            }
          }
        }
        break

      case Kind.Function:
        if (t1.kind === Kind.Function) {
          const f1 = <FunctionType> t1
          const f2 = <FunctionType> t2
          TypeVariableAllocation.assign(f1.input, f2.input, allocation)
          TypeVariableAllocation.assign(f1.output, f2.output, allocation)
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

      case Kind.Shape:
        if (t1.kind === Kind.Shape) {
          const s1 = <ShapeType> t1
          const s2 = <ShapeType> t2
          for (const p2Name of Object.keys(s2.propertyTypes)) {
            const p1Type = s1.propertyTypes[p2Name]
            if (p1Type) {
              TypeVariableAllocation.assign(p1Type, s2.propertyTypes[p2Name], allocation)
            }
          }
        } else if (t1.kind === Kind.Struct) {
          const s1 = <StructType> t1
          const s2 = <ShapeType> t2
          for (const p2Name of Object.keys(s2.propertyTypes)) {
            const p1Type = Struct.getPropertyType(s1, p2Name)
            if (p1Type) {
              TypeVariableAllocation.assign(p1Type, s2.propertyTypes[p2Name], allocation)
            }
          }
        }
        break
    }
  }
}
