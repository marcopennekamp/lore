import { IntersectionType, ProductType, SumType, Type } from './types.ts'
import { Kind } from './kinds.ts'
import { ComponentType, ListType, MapType, TypeVariable } from './types.ts'
import { areEqual } from './equality.ts'

/**
 * Checks whether t1 is a subtype of t2.
 */
export function isSubtype(t1: Type, t2: Type): boolean {
  if (t1 === t2) return true

  // How this is executed: Because multiple rules can sometimes apply to the same type pattern, we have two main
  // switch statements that match on the kinds of the left type and right type respectively. It is paramount that
  // neither switch statement returns false prematurely, as it has to be guaranteed that when the first switch can't
  // find any rule that holds, the second switch needs to try once more.
  // The idea with using switch instead of just using ifs is simple. An optimizing compiler will be able to generate
  // jump tables for carefully executed switch statements. TODO: Verify this.

  // These rules match on the left type. They are all exclusive of each other.
  switch (t1.kind) {
    case Kind.TypeVariable:
      if (isSubtype((<TypeVariable> t1).upperBound, t2)) return true
      break
    case Kind.Nothing:
      return true
    case Kind.Int:
      if (t2.kind === Kind.Real) return true
      break
    case Kind.Class:
    case Kind.Label:
      // TODO: Implement these two cases
      // d1.supertype.exists(isSubtype(_, d2))
      // { case (e1: ClassType, p2: ComponentType) if e1.isEntity => e1.componentTypes.exists(p1 => isSubtype(p1, p2)) },
      break
    case Kind.Intersection:
      if (t2.kind === Kind.Intersection) {
        if (intersectionSubtypeIntersection(<IntersectionType> t1, <IntersectionType> t2)) return true
      } else {
        if (intersectionSubtypeType(<IntersectionType> t1, t2)) return true
      }
      break
    case Kind.Sum:
      if (t2.kind === Kind.Sum) {
        if (sumSubtypeSum(<SumType> t1, <SumType> t2)) return true
      } else {
        if (sumSubtypeType(<SumType> t1, t2)) return true
      }
      break
    case Kind.Product:
      if (t2.kind === Kind.Product && productSubtypeProduct(<ProductType> t1, <ProductType> t2)) return true
      break
    case Kind.Component:
      if (
        t2.kind === Kind.Component &&
        isSubtype((<ComponentType> t1).underlying, (<ComponentType> t2).underlying)
      ) {
        return true
      }
      break
    case Kind.List:
      if (
        t2.kind === Kind.List &&
        isSubtype((<ListType> t1).element, (<ListType> t2).element)
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
  }

  // These rules match on the right type. They are also exclusive of each other.
  switch (t2.kind) {
    case Kind.TypeVariable:
      if (isSubtype(t1, (<TypeVariable> t2).lowerBound)) return true
      break
    case Kind.Any:
      return true
    case Kind.Intersection:
      // t1 could be an intersection type, but then we'd already have checked it in the first switch and since we have
      // arrived here, it is clear that in that case the answer was not true. So we can safely only apply this rule
      // if t1 is not an intersection type.
      if (t1.kind !== Kind.Intersection) {
        if (typeSubtypeIntersection(t1, <IntersectionType> t2)) return true
      }
      break
    case Kind.Sum:
      // t1 could be a sum type, but then we'd already have checked it in the first switch and since we have
      // arrived here, it is clear that in that case the answer was not true. So we can safely only apply this rule
      // if t1 is not a sum type.
      if (t1.kind !== Kind.Sum) {
        if (typeSubtypeSum(t1, <SumType> t2)) return true
      }
      break
  }

  return false
}

/**
 * Whether intersection type i1 is a subtype of intersection type i2.
 */
function intersectionSubtypeIntersection(i1: IntersectionType, i2: IntersectionType): boolean {
  const types2 = i2.types
  for (let i = 0; i < types2.length; i += 1) {
    const e2 = types2[i]
    if (!intersectionSubtypeType(i1, e2)) return false
  }
  return true
}

/**
 * Whether intersection type i1 is a subtype of type t2 (must not be a intersection type).
 */
function intersectionSubtypeType(i1: IntersectionType, t2: Type): boolean {
  const types1 = i1.types
  for (let i = 0; i < types1.length; i += 1) {
    const e1 = types1[i]
    if (isSubtype(e1, t2)) return true
  }
  return false
}

/**
 * Whether type t1 (must not be a intersection type) is a subtype of intersection type i2.
 */
function typeSubtypeIntersection(t1: Type, i2: IntersectionType): boolean {
  const types2 = i2.types
  for (let i = 0; i < types2.length; i += 1) {
    const e2 = types2[i]
    if (!isSubtype(t1, e2)) return false
  }
  return true
}

/**
 * Whether sum type s1 is a subtype of sum type s2.
 */
function sumSubtypeSum(s1: SumType, s2: SumType): boolean {
  const types1 = s1.types
  for (let i = 0; i < types1.length; i += 1) {
    const e1 = types1[i]
    if (!typeSubtypeSum(e1, s2)) return false
  }
  return true
}

/**
 * Whether sum type s1 is a subtype of type t2 (must not be a sum type).
 */
function sumSubtypeType(s1: SumType, t2: Type): boolean {
  const types1 = s1.types
  for (let i = 0; i < types1.length; i += 1) {
    const e1 = types1[i]
    if (!isSubtype(e1, t2)) return false
  }
  return true
}

/**
 * Whether type t1 (must not be a sum type) is a subtype of sum type s2.
 */
function typeSubtypeSum(t1: Type, s2: SumType): boolean {
  const types2 = s2.types
  for (let i = 0; i < types2.length; i += 1) {
    const e2 = types2[i]
    if (isSubtype(t1, e2)) return true
  }
  return false
}

/**
 * Whether product type p1 is a subtype of product type p2.
 */
function productSubtypeProduct(p1: ProductType, p2: ProductType): boolean {
  const types1 = p1.types
  const types2 = p2.types
  if (types1.length !== types2.length) return false

  for (let i = 0; i < types1.length; i += 1) {
    const e1 = types1[i]
    const e2 = types2[i]
    if (!isSubtype(e1, e2)) return false
  }

  return true
}
