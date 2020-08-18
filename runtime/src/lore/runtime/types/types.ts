import { Kind } from './kinds.ts'
import {
  Hashed,
  orderedHashWithSeed,
  pairHash,
  singleHash,
  stringHash,
  stringHashWithSeed,
  unorderedHashWithSeed,
} from '../utils/hash.ts'

export interface Type extends Hashed {
  kind: Kind
}


export interface AnyType extends Type { }
export const any: AnyType = { kind: Kind.Any, hash: stringHash("any") }

export interface NothingType extends Type { }
export const nothing: NothingType = { kind: Kind.Nothing, hash: stringHash("nothing") }

export interface RealType extends Type { }
export const real: RealType = { kind: Kind.Real, hash: stringHash("real") }

export interface IntType extends Type { }
export const int: IntType = { kind: Kind.Int, hash: stringHash("int") }

export interface BooleanType extends Type { }
export const boolean: BooleanType = { kind: Kind.Boolean, hash: stringHash("boolean") }

export interface StringType extends Type { }
export const string: StringType = { kind: Kind.String, hash: stringHash("string") }


export interface TypeVariable extends Type {
  name: string
  lowerBound: Type
  upperBound: Type
}

export function variable(name: string, lowerBound: Type, upperBound: Type): TypeVariable {
  // TODO: Can we make the hash not only dependent on the variable name?
  return { kind: Kind.TypeVariable, name, lowerBound, upperBound, hash: stringHashWithSeed(name, 0x7ff08f15) }
}


export interface XaryType extends Type {
  types: Array<Type>
}


export interface IntersectionType extends XaryType { }

export function intersection(types: Array<Type>): IntersectionType {
  return { kind: Kind.Intersection, types, hash: unorderedHashWithSeed(types, 0x74a2317d) }
}

export interface SumType extends XaryType { }

export function sum(types: Array<Type>): SumType {
  return { kind: Kind.Sum, types, hash: unorderedHashWithSeed(types, 0x85f5fe35) }
}

export interface ProductType extends XaryType { }

export function product(types: Array<Type>): ProductType {
  return { kind: Kind.Product, types, hash: orderedHashWithSeed(types, 0x4baf1ec8) }
}

/**
 * Creates a product type WITHOUT a sensible hash. This should ONLY be used by the compiler to optimize
 * operations that don't require a hash, such as multiple dispatch resolution with a disabled cache.
 */
export function unhashedProduct(types: Array<Type>): ProductType {
  return { kind: Kind.Product, types, hash: 0 }
}

export const unit: ProductType = product([])


export interface ComponentType extends Type {
  underlying: ClassType
}

export function component(underlying: ClassType): ComponentType {
  if (underlying.kind !== Kind.Class) {
    throw Error("A component type must have an underlying class type.")
  }
  return { kind: Kind.Component, underlying, hash: singleHash(underlying, 0x4cab1ec0) }
}


export interface ListType extends Type {
  element: Type
}

export function list(element: Type): ListType {
  return { kind: Kind.List, element, hash: singleHash(element, 0xfb04146c) }
}


export interface MapType extends Type {
  key: Type
  value: Type
}

export function map(key: Type, value: Type): MapType {
  return { kind: Kind.Map, key, value, hash: pairHash(key, value, 0xbeda0294) }
}


export interface ClassType extends Type {
  name: string
  supertype?: ClassType
  ownedBy?: Type
  componentTypes: Array<ComponentType>
  isEntity: Boolean
}

export function classType(
  name: string, supertype: ClassType | undefined, ownedBy: Type | undefined, componentTypes: Array<ComponentType>, isEntity: boolean,
): ClassType {
  return { kind: Kind.Class, name, supertype, ownedBy, componentTypes, isEntity, hash: stringHashWithSeed(name, 0x38ba128e) }
}


export interface LabelType extends Type {
  name: string
}
