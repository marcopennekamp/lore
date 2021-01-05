import {
  DeclaredType,
  IntersectionType,
  ListType,
  MapType,
  ProductType,
  ShapeType,
  Structs,
  StructType,
  SumType,
  Type,
  TypeVariable,
} from './types.ts'
import { Kind } from "./kinds.ts";
import { areEqual } from "./equality.ts";

/**
 * A subtyping environment provides a specific implementation of the isSubtype function given a configuration.
 *
 * TODO: Since there are no current configurations, we can remove the environment again.
 */
export class SubtypingEnvironment {
  constructor() {
  }

  // TODO (shape): Implementing structural subtyping for struct < shape: Take all properties defined in the shape
  //               and get their types by: (1) checking the openPropertyTypes map and (2) the type of the
  //               PropertyDefinition.

  /**
   * Checks whether t1 is a subtype of t2.
   */
  isSubtype(t1: Type, t2: Type): boolean {
    if (t1 === t2) return true

    // How this is executed: Because multiple rules can sometimes apply to the same type pattern, we have two main
    // switch statements that match on the kinds of the left type and right type respectively. It is paramount that
    // neither switch statement returns false prematurely, as it has to be guaranteed that when the first switch can't
    // find any rule that holds, the second switch needs to try once more.

    // These rules match on the left type. They are all exclusive of each other.
    switch (t1.kind) {
      case Kind.TypeVariable:
        if (this.isSubtype((<TypeVariable> t1).upperBound, t2)) return true
        break

      case Kind.Nothing:
        return true

      case Kind.Int:
        if (t2.kind === Kind.Real) return true
        break

      case Kind.Struct:
        // Handle shape subtyping specifically for structs.
        if (t2.kind === Kind.Shape && this.structSubtypeShape(<StructType> t1, <ShapeType> t2)) {
          return true
        }
        // fallthrough
      case Kind.Trait:
        const d1 = <DeclaredType> t1
        if (t2.kind === Kind.Trait || t2.kind === Kind.Struct) {
          const d2 = <DeclaredType> t2

          // TODO: Rebuild this for open property types (structs only, though):

          /* // If the schemas of these two declared types are equal, we have the same type most of the time. The equality
          // might not have been caught by the === check above because a struct type can have multiple instances with
          // different actual (open) property types.
          // We also have to take component types into account when deciding subtyping, however. Let's say we have a
          // struct a1: A with a component C1 and another struct a2: A with a component C2. Given C2 < C1, is a1's
          // struct type a subtype of a2's struct type?
          // It is not. We couldn't assign a1 to a variable that explicitly expects a type like a2. This is of no
          // importance at compile-time, of course, since struct types are all "archetypes" then. But to get sound
          // run-time subtyping, we also have to look at the components.
          // Fortunately, we only have to do that if the right-hand type t2 is not an archetype and only if the type
          // is an entity, of course. Otherwise, checking the schema will suffice.
          if (d1.schema === d2.schema) {
            // We could "optimize" this check by requiring d2 to be a struct, but equal traits are already caught
            // by the reference equality check above. Also, d1 will almost never be a trait, so if the schemas are
            // equal, we can already be almost 100% certain that we are dealing with structs.
            if (!d2.isArchetype) {
              const componentTypes1 = d1.componentTypes
              const componentTypes2 = d2.componentTypes
              for (let i = 0; i < componentTypes1.length; i += 1) {
                if (!this.isSubtype(componentTypes1[i], componentTypes2[i])) return false
              }
            }

            return true
          } */

          const supertraits = d1.schema.supertraits
          for (let i = 0; i < supertraits.length; i += 1) {
            if (this.isSubtype(supertraits[i], d2)) return true
          }
        }
        break

      case Kind.Intersection:
        if (t2.kind === Kind.Intersection) {
          if (this.intersectionSubtypeIntersection(<IntersectionType> t1, <IntersectionType> t2)) return true
        } else {
          if (this.intersectionSubtypeType(<IntersectionType> t1, t2)) return true
        }
        break

      case Kind.Sum:
        if (t2.kind === Kind.Sum) {
          if (this.sumSubtypeSum(<SumType> t1, <SumType> t2)) return true
        } else {
          if (this.sumSubtypeType(<SumType> t1, t2)) return true
        }
        break

      case Kind.Product:
        if (t2.kind === Kind.Product && this.productSubtypeProduct(<ProductType> t1, <ProductType> t2)) return true
        break

      case Kind.List:
        if (
            t2.kind === Kind.List &&
            this.isSubtype((<ListType> t1).element, (<ListType> t2).element)
        ) {
          return true
        }
        break

      case Kind.Map:
        if (
            t2.kind === Kind.Map &&
            areEqual((<MapType> t1).key, (<MapType> t2).key) &&
            areEqual((<MapType> t1).value, (<MapType> t2).value)
        ) {
          return true
        }
        break

      // This needs to be placed lower than the Kind.Struct case, because structs handle struct/shape subtyping.
      case Kind.Shape:
        if (t2.kind === Kind.Shape && this.shapeSubtypeShape(<ShapeType> t1, <ShapeType> t2)) {
          return true
        }
        break
    }

    // These rules match on the right type. They are also exclusive of each other.
    switch (t2.kind) {
      case Kind.TypeVariable:
        if (this.isSubtype(t1, (<TypeVariable> t2).lowerBound)) return true
        break

      case Kind.Any:
        return true

      case Kind.Intersection:
        // t1 could be an intersection type, but then we'd already have checked it in the first switch and since we have
        // arrived here, it is clear that in that case the answer was not true. So we can safely only apply this rule
        // if t1 is not an intersection type.
        if (t1.kind !== Kind.Intersection) {
          if (this.typeSubtypeIntersection(t1, <IntersectionType> t2)) return true
        }
        break

      case Kind.Sum:
        // t1 could be a sum type, but then we'd already have checked it in the first switch and since we have
        // arrived here, it is clear that in that case the answer was not true. So we can safely only apply this rule
        // if t1 is not a sum type.
        if (t1.kind !== Kind.Sum) {
          if (this.typeSubtypeSum(t1, <SumType> t2)) return true
        }
        break
    }

    return false
  }

  /**
   * Whether intersection type i1 is a subtype of intersection type i2.
   */
  private intersectionSubtypeIntersection(i1: IntersectionType, i2: IntersectionType): boolean {
    const types2 = i2.types
    for (let i = 0; i < types2.length; i += 1) {
      const e2 = types2[i]
      if (!this.intersectionSubtypeType(i1, e2)) return false
    }
    return true
  }

  /**
   * Whether intersection type i1 is a subtype of type t2 (must not be a intersection type).
   */
  private intersectionSubtypeType(i1: IntersectionType, t2: Type): boolean {
    const types1 = i1.types
    for (let i = 0; i < types1.length; i += 1) {
      const e1 = types1[i]
      if (this.isSubtype(e1, t2)) return true
    }
    return false
  }

  /**
   * Whether type t1 (must not be a intersection type) is a subtype of intersection type i2.
   */
  private typeSubtypeIntersection(t1: Type, i2: IntersectionType): boolean {
    const types2 = i2.types
    for (let i = 0; i < types2.length; i += 1) {
      const e2 = types2[i]
      if (!this.isSubtype(t1, e2)) return false
    }
    return true
  }

  /**
   * Whether sum type s1 is a subtype of sum type s2.
   */
  private sumSubtypeSum(s1: SumType, s2: SumType): boolean {
    const types1 = s1.types
    for (let i = 0; i < types1.length; i += 1) {
      const e1 = types1[i]
      if (!this.typeSubtypeSum(e1, s2)) return false
    }
    return true
  }

  /**
   * Whether sum type s1 is a subtype of type t2 (must not be a sum type).
   */
  private sumSubtypeType(s1: SumType, t2: Type): boolean {
    const types1 = s1.types
    for (let i = 0; i < types1.length; i += 1) {
      const e1 = types1[i]
      if (!this.isSubtype(e1, t2)) return false
    }
    return true
  }

  /**
   * Whether type t1 (must not be a sum type) is a subtype of sum type s2.
   */
  private typeSubtypeSum(t1: Type, s2: SumType): boolean {
    const types2 = s2.types
    for (let i = 0; i < types2.length; i += 1) {
      const e2 = types2[i]
      if (this.isSubtype(t1, e2)) return true
    }
    return false
  }

  /**
   * Whether product type p1 is a subtype of product type p2.
   */
  private productSubtypeProduct(p1: ProductType, p2: ProductType): boolean {
    const types1 = p1.types
    const types2 = p2.types
    if (types1.length !== types2.length) return false

    for (let i = 0; i < types1.length; i += 1) {
      const e1 = types1[i]
      const e2 = types2[i]
      if (!this.isSubtype(e1, e2)) return false
    }

    return true
  }

  /**
   * Whether struct type s1 is a subtype of shape type s2.
   */
  private structSubtypeShape(s1: StructType, s2: ShapeType): boolean {
    for (const p2Name of Object.keys(s2.propertyTypes)) {
      const p1Type = Structs.getPropertyType(s1, p2Name)
      if (!p1Type || !this.isSubtype(p1Type, s2.propertyTypes[p2Name])) {
        return false
      }
    }
    return true
  }

  /**
   * Whether shape type s1 is a subtype of shape type s2.
   */
  private shapeSubtypeShape(s1: ShapeType, s2: ShapeType): boolean {
    for (const p2Name of Object.keys(s2.propertyTypes)) {
      const p1Type = s1.propertyTypes[p2Name]
      if (!p1Type || !this.isSubtype(p1Type, s2.propertyTypes[p2Name])) {
        return false
      }
    }
    return true
  }
}
