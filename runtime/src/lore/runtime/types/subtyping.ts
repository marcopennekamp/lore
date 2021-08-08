import { FunctionType } from '../functions.ts'
import { Intersection, IntersectionType } from '../intersections.ts'
import { ListType } from '../lists.ts'
import { MapType } from '../maps.ts'
import { ShapeType } from '../shapes.ts'
import { Struct, StructType } from '../structs.ts'
import { Sum, SumType } from '../sums.ts'
import { Trait, TraitSchema, TraitType } from '../traits.ts'
import { TupleType } from '../tuples.ts'
import { DeclaredSchemas } from './declared-schemas.ts'
import { DeclaredType } from './declared-types.ts'
import { areEqual } from './equality.ts'
import { Kind } from './kinds.ts'
import { PropertyTypes } from './property-types.ts'
import { Assignments, TypeVariable, Variance } from './type-variables.ts'
import { Type } from './types.ts'
import { unique } from './util.ts'

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

    case Kind.Trait:
    case Kind.Struct:
      if (t2.kind === Kind.Shape) {
        if (t1.kind === Kind.Struct && structSubtypeShape(<StructType> t1, <ShapeType> t2)) {
          return true
        } else if (t1.kind === Kind.Trait && isSubtype(Trait.getInheritedShapeType(<TraitType> t1), t2)) {
          return true
        }
      } else if (t2.kind === Kind.Trait) {
        return declaredTypeSubtypeTrait(<DeclaredType> t1, <TraitType> t2)
      } else if (t2.kind === Kind.Struct) {
        // If t2 is a struct type, we don't have to check whether any of t1's supertraits are equal to t2, because
        // a struct cannot be a supertrait. Because t1 and t2 aren't referentially equal either, the only route to
        // subtyping is through checking type arguments and open property types.
        if (t1.kind === Kind.Struct) {
          const s1 = <StructType> t1
          const s2 = <StructType> t2
          if (s1.schema === s2.schema && checkSubtypeTypeArguments(s1, s2)) {
            // If the open property types of s2 are empty, and s1 and s2 agree in their type arguments, s2 will always
            // be a supertype of s1. Each property type of s2 is as general as possible.
            if (!s2.propertyTypes) {
              return true
            }

            // This subtyping check assumes that open property types are either undefined or fully defined. This
            // allows us to check keys in only one direction.
            return structSubtypePropertyTypes(s1, s2.propertyTypes)
          }
        }
        return false
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
        return tupleSubtypeTuple(<TupleType> t1, <TupleType> t2)
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
 * Whether d1's type arguments subtype d2's type arguments. Both types must have the same schema.
 */
function checkSubtypeTypeArguments(d1: DeclaredType, d2: DeclaredType): boolean {
  const parameters = d1.schema.typeParameters
  if (parameters.length === 0) {
    // The common schema is constant and thus the two types are equal.
    return true
  }

  const arguments1 = <Assignments> d1.typeArguments
  const arguments2 = <Assignments> d2.typeArguments
  for (let i = 0; i < parameters.length; i += 1) {
    const tv = parameters[i]
    const t1 = arguments1[i]
    const t2 = arguments2[i]

    switch (tv.variance) {
      case Variance.Covariant:
        if (!isSubtype(t1, t2)) return false
        break

      case Variance.Contravariant:
        if (!isSubtype(t2, t1)) return false
        break

      case Variance.Invariant:
        if (!areEqual(t1, t2)) return false
        break
    }
  }

  return true
}

/**
 * Whether d1 is a subtype of t2.
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
 */
function declaredTypeSubtypeTrait(d1: DeclaredType, t2: TraitType): boolean {
  const schema1 = d1.schema
  const schema2 = t2.schema
  if (schema1 === schema2) {
    return checkSubtypeTypeArguments(d1, t2)
  }

  let supertrait
  if (!schema1.hasMultipleParameterizedInheritance || DeclaredSchemas.isConstant(schema2)) {
    supertrait = findFirstSupertrait(d1, schema2)
  } else {
    supertrait = getCombinedSupertrait(d1, schema2)
  }

  return !!supertrait && isSubtype(supertrait, t2)
}

/**
 * Finds the first occurrence of schema2 in d1's supertraits. This operation is only applicable if the d1's schema's
 * `hasMultipleParameterizedInheritance` flag is set to `false` or if schema2 is constant.
 */
function findFirstSupertrait(d1: DeclaredType, schema2: TraitSchema): TraitType | undefined {
  const queue = d1.supertraits.slice()
  while (queue.length > 0) {
    const candidate = <TraitType> queue.pop()
    if (candidate.schema === schema2) {
      return candidate
    }
    queue.push(...candidate.supertraits)
  }
  return undefined
}

/**
 * Finds all occurrences of schema2 in d1's supertraits. This function is only used when schema2 has type parameters.
 *
 * TODO (schemas): This is a very heavy algorithm. We definitely have to cache this according to the ideas mentioned in
 *                 `declaredTypeSubtypeTrait`.
 */
function getCombinedSupertrait(d1: DeclaredType, schema2: TraitSchema): TraitType | undefined {
  const candidates: Array<TraitType> = []
  const collect = (dt: DeclaredType) => {
    if (dt.schema === schema2) {
      candidates.push(<TraitType> dt)
    } else {
      for (const supertype of dt.supertraits) {
        collect(supertype)
      }
    }
  }
  collect(d1)

  if (candidates.length <= 1) {
    return candidates[0]
  }

  const parameters = schema2.typeParameters
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

  return Trait.type(schema2, combinedArguments)
}

/**
 * Whether all property types of the struct are subtypes of the given property types.
 */
function structSubtypePropertyTypes(structType: StructType, propertyTypes: PropertyTypes): boolean {
  for (const p2Name of Object.keys(propertyTypes)) {
    const p1Type = Struct.getPropertyType(structType, p2Name)
    if (!p1Type || !isSubtype(p1Type, propertyTypes[p2Name])) {
      return false
    }
  }
  return true
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
function tupleSubtypeTuple(t1: TupleType, t2: TupleType): boolean {
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

/**
 * Whether struct type s1 is a subtype of shape type s2.
 */
function structSubtypeShape(s1: StructType, s2: ShapeType): boolean {
  return structSubtypePropertyTypes(s1, s2.propertyTypes)
}
