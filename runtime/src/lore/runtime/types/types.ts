import { Kind } from './kinds.ts'
import { murmurhash3 } from '../utils/hash.ts'

export interface Type {
  kind: Kind
  hash: number
  // TODO: Add a hash code which is calculated when the type is constructed. No shenanigans.
}

// TODO: Optimize this for different kinds of types.
const HASH_SEED = 0xcafebabe

export const any: Type = { kind: Kind.Any, hash: murmurhash3("any", HASH_SEED) }
export const nothing: Type = { kind: Kind.Nothing, hash: murmurhash3("nothing", HASH_SEED) }
export const real: Type = { kind: Kind.Real, hash: murmurhash3("real", HASH_SEED) }
export const int: Type = { kind: Kind.Int, hash: murmurhash3("int", HASH_SEED) }
export const boolean: Type = { kind: Kind.Boolean, hash: murmurhash3("boolean", HASH_SEED) }
export const string: Type = { kind: Kind.String, hash: murmurhash3("string", HASH_SEED) }
export const unit: ProductType = product([])


export interface TypeVariable extends Type {
  name: string
  lowerBound: Type
  upperBound: Type
}

export function variable(name: string, lowerBound: Type, upperBound: Type): TypeVariable {
  // TODO: Can we make the hash not only dependent on the variable name?
  return { kind: Kind.TypeVariable, name, lowerBound, upperBound, hash: murmurhash3(name, HASH_SEED) }
}


export interface XaryType extends Type {
  types: Array<Type>
}

export interface IntersectionType extends XaryType { }

export function intersection(types: Array<Type>): IntersectionType {
  // TODO: Actually hash this...
  return { kind: Kind.Intersection, types, hash: murmurhash3(types.map(t => t.hash).toString(), HASH_SEED) }
}

export interface SumType extends XaryType { }

export function sum(types: Array<Type>): SumType {
  // TODO: Actually hash this...
  return { kind: Kind.Sum, types, hash: murmurhash3(types.map(t => t.hash).toString(), HASH_SEED) }
}

export interface ProductType extends XaryType { }

export function product(types: Array<Type>): ProductType {
  // TODO: Actually hash this...
  const p = { kind: Kind.Product, types, hash: murmurhash3(types.map(t => t.hash).toString(), HASH_SEED) }
  //console.log(p.hash)
  return p
}


export interface ComponentType extends Type {
  underlying: Type // TODO: Change to ClassType.
}

export function component(underlying: Type): ComponentType {
  if (underlying.kind !== Kind.Class) {
    throw Error("A component type must have an underlying class type.")
  }

  // TODO: This way of hashing must be laughably bad.
  return { kind: Kind.Component, underlying, hash: murmurhash3(underlying.hash + '', HASH_SEED) }
}


export interface ListType extends Type {
  element: Type
}

export function list(element: Type): ListType {
  return { kind: Kind.List, element, hash: murmurhash3(element.hash + '', HASH_SEED) }
}


export interface MapType extends Type {
  key: Type
  value: Type
}

export function map(key: Type, value: Type): MapType {
  return { kind: Kind.Map, key, value, hash: murmurhash3(key.hash + '' + value.hash, HASH_SEED) }
}
