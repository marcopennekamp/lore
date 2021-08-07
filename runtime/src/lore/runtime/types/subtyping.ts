import { FunctionType } from '../functions.ts'
import { IntersectionType } from '../intersections.ts'
import { ListType } from '../lists.ts'
import { MapType } from '../maps.ts'
import { ShapeType } from '../shapes.ts'
import { Struct, StructType } from '../structs.ts'
import { SumType } from '../sums.ts'
import { TraitType } from '../traits.ts'
import { TupleType } from '../tuples.ts'
import { DeclaredType } from './declared-types.ts'
import { areEqual } from './equality.ts'
import { Kind } from './kinds.ts'
import { PropertyTypes } from './property-types.ts'
import { TypeVariable } from './type-variables.ts'
import { Type } from './types.ts'

/**
 * Checks whether t1 is a subtype of t2.
 */
export function isSubtype(t1: Type, t2: Type): boolean {
  if (t1 === t2) return true

  // How this is executed: Because multiple rules can sometimes apply to the same type pattern, we have two main
  // switch statements that match on the kinds of the left type and right type respectively. It is paramount that
  // neither switch statement returns false prematurely, as it has to be guaranteed that when the first switch can't
  // find any rule that holds, the second switch needs to try once more.

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

    // TODO (schemas): Remember to honor the flag `hasMultipleParameterizedInheritance`.
    case Kind.Trait:
    case Kind.Struct:
      if (t2.kind === Kind.Shape) {
        if (t1.kind === Kind.Struct && structSubtypeShape(<StructType> t1, <ShapeType> t2)) {
          return true
        } else if (t1.kind === Kind.Trait && isSubtype((<TraitType> t1).schema.inheritedShapeType.value(), t2)) {
          return true
        }
      } else if (t2.kind === Kind.Struct) {
        // If t2 is a struct type, we don't have to check whether any of t1's supertraits are equal to t2, because
        // a struct cannot be a supertrait. Because t1 and t2 aren't referentially equal either, the only route to
        // subtyping is through the open property types.
        if (t1.kind === Kind.Struct) {
          const s1 = <StructType> t1
          const s2 = <StructType> t2
          if (s1.schema === s2.schema) {
            // The representative is always a supertype of all the structs that could possibly be instantiated,
            // because each property type of the representative is the property's upper bound.
            // Note that we can't trivially assume the reverse and return false if s1 is a representative. Struct
            // type s2 might not be the representative, but its open property types might still be equal to the
            // property types of the representative.
            // TODO (schemas): This is only valid if s2's schema is constant.
            if (s2 === s2.schema.representative) {
              return true
            }

            // This subtyping check assumes that open property types are either undefined or fully defined. This
            // allows us to check keys in only one direction.
            if (s1.propertyTypes) {
              return propertyTypesSubtypeStruct(s1.propertyTypes, s2)
            } else if (s2.propertyTypes) {
              return structSubtypesPropertyTypes(s1, s2.propertyTypes)
            }
          }
        }
        return false
      } else if (t2.kind === Kind.Trait) {
        const d1 = <DeclaredType> t1
        const d2 = <DeclaredType> t2

        const supertraits = d1.schema.supertraits
        for (let i = 0; i < supertraits.length; i += 1) {
          if (isSubtype(supertraits[i], d2)) return true
        }
      }
      break

    case Kind.Sum:
      if (t2.kind === Kind.Sum) {
        return sumSubtypeSum(<SumType> t1, <SumType> t2)
      } else {
        if (sumSubtypeType(<SumType> t1, t2)) return true
      }
      break

    case Kind.Intersection:
      if (t2.kind === Kind.Intersection) {
        return intersectionSubtypeIntersection(<IntersectionType> t1, <IntersectionType> t2)
      } else {
        if (intersectionSubtypeType(<IntersectionType> t1, t2)) return true
      }
      break

    case Kind.Tuple:
      if (t2.kind === Kind.Tuple) {
        return tupleSubtypesTuple(<TupleType> t1, <TupleType> t2)
      }
      break

    case Kind.Function:
      if (t2.kind === Kind.Function) {
        const f1 = <FunctionType> t1
        const f2 = <FunctionType> t2
        return isSubtype(f2.input, f1.input) && isSubtype(f1.output, f2.output)
      }
      break

    case Kind.List:
      if (t2.kind === Kind.List) {
        return isSubtype((<ListType> t1).element, (<ListType> t2).element)
      }
      break

    case Kind.Map:
      if (t2.kind === Kind.Map) {
        const m1 = <MapType> t1
        const m2 = <MapType> t2
        return areEqual(m1.key, m2.key) && areEqual(m1.value, m2.value)
      }
      break

    case Kind.Shape:
      if (t2.kind === Kind.Shape) {
        return shapeSubtypeShape(<ShapeType> t1, <ShapeType> t2)
      }
      break

    // There is no need to handle symbol types here. s1 can only be a subtype of s2 if s1 === s2, because symbol
    // types are interned.
  }

  // These rules match on the right type. They are also exclusive of each other.
  switch (t2.kind) {
    case Kind.TypeVariable:
      if (isSubtype(t1, (<TypeVariable> t2).lowerBound)) return true
      break

    case Kind.Any:
      return true

    case Kind.Sum:
      // t1 could be a sum type, but then we'd already have checked it in the first switch and since we have
      // arrived here, it is clear that in that case the answer was not true. So we can safely only apply this rule
      // if t1 is not a sum type.
      if (t1.kind !== Kind.Sum) {
        if (typeSubtypeSum(t1, <SumType> t2)) return true
      }
      break

    case Kind.Intersection:
      // t1 could be an intersection type, but then we'd already have checked it in the first switch and since we have
      // arrived here, it is clear that in that case the answer was not true. So we can safely only apply this rule
      // if t1 is not an intersection type.
      if (t1.kind !== Kind.Intersection) {
        if (typeSubtypeIntersection(t1, <IntersectionType> t2)) return true
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
 * Whether tuple type t1 is a subtype of product type t2.
 */
function tupleSubtypesTuple(t1: TupleType, t2: TupleType): boolean {
  const types1 = t1.types
  const types2 = t2.types
  if (types1.length !== types2.length) return false

  for (let i = 0; i < types1.length; i += 1) {
    const e1 = types1[i]
    const e2 = types2[i]
    if (!isSubtype(e1, e2)) return false
  }

  return true
}

/**
 * Whether struct type s1 is a subtype of shape type s2.
 */
function structSubtypeShape(s1: StructType, s2: ShapeType): boolean {
  return structSubtypesPropertyTypes(s1, s2.propertyTypes)
}

/**
 * Whether all given property types are supertypes of their respective struct property types.
 */
function structSubtypesPropertyTypes(structType: StructType, propertyTypes: PropertyTypes): boolean {
  for (const p2Name of Object.keys(propertyTypes)) {
    const p1Type = Struct.getPropertyType(structType, p2Name)
    if (!p1Type || !isSubtype(p1Type, propertyTypes[p2Name])) {
      return false
    }
  }
  return true
}

/**
 * Whether all given property types are subtypes of their respective struct property types.
 */
function propertyTypesSubtypeStruct(propertyTypes: PropertyTypes, structType: StructType): boolean {
  for (const p1Name of Object.keys(propertyTypes)) {
    const p2Type = Struct.getPropertyType(structType, p1Name)
    if (!p2Type || !isSubtype(propertyTypes[p1Name], p2Type)) {
      return false
    }
  }
  return true
}

/**
 * Whether shape type s1 is a subtype of shape type s2.
 */
function shapeSubtypeShape(s1: ShapeType, s2: ShapeType): boolean {
  for (const p2Name of Object.keys(s2.propertyTypes)) {
    const p1Type = s1.propertyTypes[p2Name]
    if (!p1Type || !isSubtype(p1Type, s2.propertyTypes[p2Name])) {
      return false
    }
  }
  return true
}
