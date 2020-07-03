import { Kind } from './kinds.ts'
import {
  Hashed,
  singleHash,
  orderedHashWithSeed,
  stringHash,
  stringHashWithSeed,
  unorderedHashWithSeed,
  pairHash,
} from '../utils/hash.ts'

export interface Type extends Hashed {
  kind: Kind
}


export const any: Type = { kind: Kind.Any, hash: stringHash("any") }
export const nothing: Type = { kind: Kind.Nothing, hash: stringHash("nothing") }
export const real: Type = { kind: Kind.Real, hash: stringHash("real") }
export const int: Type = { kind: Kind.Int, hash: stringHash("int") }
export const boolean: Type = { kind: Kind.Boolean, hash: stringHash("boolean") }
export const string: Type = { kind: Kind.String, hash: stringHash("string") }
export const unit: ProductType = product([])


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
  // TODO: Actually hash this...
  return { kind: Kind.Intersection, types, hash: unorderedHashWithSeed(types, 0x74a2317d) }
}

export interface SumType extends XaryType { }

export function sum(types: Array<Type>): SumType {
  // TODO: Actually hash this...
  return { kind: Kind.Sum, types, hash: unorderedHashWithSeed(types, 0x85f5fe35) }
}

export interface ProductType extends XaryType { }

export function product(types: Array<Type>): ProductType {
  return { kind: Kind.Product, types, hash: orderedHashWithSeed(types, 0x4baf1ec8) }
}


export interface ComponentType extends Type {
  underlying: Type // TODO: Change to ClassType.
}

export function component(underlying: Type): ComponentType {
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
