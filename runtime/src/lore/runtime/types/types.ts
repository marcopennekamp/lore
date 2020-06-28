import { Kind } from './kinds.ts'

export interface Type {
  kind: Kind
}

export const any: Type = { kind: Kind.Any }
export const nothing: Type = { kind: Kind.Nothing }
export const real: Type = { kind: Kind.Real }
export const int: Type = { kind: Kind.Int }
export const boolean: Type = { kind: Kind.Boolean }
export const string: Type = { kind: Kind.String }
export const unit: ProductType = product([])


export interface TypeVariable extends Type {
  name: string
  lowerBound: Type
  upperBound: Type
}

export function variable(name: string, lowerBound: Type, upperBound: Type): TypeVariable {
  return { kind: Kind.TypeVariable, name, lowerBound, upperBound }
}


export interface XaryType extends Type {
  types: Array<Type>
}

export interface IntersectionType extends XaryType { }

export function intersection(types: Array<Type>): IntersectionType {
  return { kind: Kind.Intersection, types }
}

export interface SumType extends XaryType { }

export function sum(types: Array<Type>): SumType {
  return { kind: Kind.Sum, types }
}

export interface ProductType extends XaryType { }

export function product(types: Array<Type>): ProductType {
  return { kind: Kind.Product, types }
}


export interface ComponentType extends Type {
  underlying: Type // TODO: Change to ClassType.
}

export function component(underlying: Type): ComponentType {
  if (underlying.kind !== Kind.Class) {
    throw Error("A component type must have an underlying class type.")
  }

  return { kind: Kind.Component, underlying }
}


export interface ListType extends Type {
  element: Type
}

export function list(element: Type): ListType {
  return { kind: Kind.List, element }
}


export interface MapType extends Type {
  key: Type
  value: Type
}

export function map(key: Type, value: Type): MapType {
  return { kind: Kind.Map, key, value }
}
