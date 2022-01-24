import { Function, FunctionType } from './functions.ts'
import { Intersection } from './intersections.ts'
import { Lists, ListType } from './lists.ts'
import { Shape, ShapeType } from './shapes.ts'
import { Sum } from './sums.ts'
import { Tuple, TupleType } from './tuples.ts'
import { DeclaredSchema, DeclaredSchemas } from './types/declared-schemas.ts'
import { DeclaredType, InvalidTypeArguments } from './types/declared-types.ts'
import { Kind } from './types/kinds.ts'
import { isSubtype } from './types/subtyping.ts'
import { Variance } from './types/type-variables.ts'
import { Type } from './types/types.ts'
import { allExcluding, appendInner, flattenedUnique } from './types/util.ts'

/**
 * See the corresponding compiler implementation `lore.compiler.types.Simplification`.
 */
export const Simplification = {
  construct(kind: Kind, parts: Array<Type>): Type {
    if (parts.length === 1) {
      return parts[0]
    }

    const flattened = flattenedUnique(kind, parts)
    const combined = combine(flattened, kind)
    const relevants = filterRelevantTypes(combined, kind)

    if (relevants.length == 1) {
      return relevants[0]
    } else {
      switch (kind) {
        case Kind.Sum: return Sum.type(relevants)
        case Kind.Intersection: return Intersection.type(relevants)
        default: throw invalidKind(kind)
      }
    }
  },
}

function combine(parts: Array<Type>, kind: Kind): Array<Type> {
  let constructCovariant: (parts: Array<Type>) => Type
  let constructContravariant: (parts: Array<Type>) => Type

  switch (kind) {
    case Kind.Sum:
      constructCovariant = Sum.simplified
      constructContravariant = Intersection.simplified
      break

    case Kind.Intersection:
      constructCovariant = Intersection.simplified
      constructContravariant = Sum.simplified
      break

    default: throw invalidKind(kind)
  }

  const tuplesBySize: Map<number, Array<TupleType>> = new Map()
  const functions: Array<FunctionType> = []
  const lists: Array<ListType> = []
  const shapes: Array<ShapeType> = []
  const declaredTypesBySchema: Map<DeclaredSchema, Array<DeclaredType>> = new Map()
  const result: Array<Type> = []

  for (const part of parts) {
    switch (part.kind) {
      case Kind.Tuple:
        let tuple = <TupleType> part
        appendInner(tuplesBySize, tuple.types.length, tuple)
        break

      case Kind.Function:
        functions.push(<FunctionType> part)
        break

      case Kind.List:
        lists.push(<ListType> part)
        break

      case Kind.Shape:
        if (kind == Kind.Intersection) {
          shapes.push(<ShapeType> part)
        } else {
          result.push(part)
        }
        break

      case Kind.Trait:
      case Kind.Struct:
        let dt = <DeclaredType> part
        if (!dt.schema.hasInvariantParameters) {
          appendInner(declaredTypesBySchema, dt.schema, dt)
        } else {
          result.push(part)
        }
        break

      default:
        result.push(part)
    }
  }

  // (A, B) | (C, D) :=: (A | C, B | D)
  // (A, B) & (C, D) :=: (A & C, B & D)
  for (const [size, tuples] of tuplesBySize) {
    if (tuples.length === 1) {
      result.push(tuples[0])
    } else {
      const elements: Array<Type> = []
      for (let i = 0; i <= size; i += 1) {
        const elementParts = []
        for (const tuple of tuples) {
          elementParts.push(tuple.types[i])
        }
        elements.push(constructCovariant(elementParts))
      }
      result.push(Tuple.type(elements))
    }
  }

  // (A => B) | (C => D) :=: (A & C) => (B | D)
  // (A => B) & (C => D) :=: (A | C) => (B & D)
  if (functions.length > 0) {
    const input = Tuple.tupled(constructContravariant(functions.map(f => f.input)))
    const output = constructCovariant(functions.map(f => f.output))
    result.push(Function.type(input, output))
  }

  // [A] | [B] :=: [A | B]
  // [A] & [B] :=: [A & B]
  if (lists.length > 0) {
    const element = constructCovariant(lists.map(t => t.element))
    result.push(Lists.type(element))
  }

  // { name: A } & { name: B } & { health: Int }  ==  { name: A & B, health: Int }
  if (shapes.length > 0) {
    result.push(Shape.combine(shapes))
  }

  // Given a declared type `D[+X, -Y]`:
  // D[A, B] | D[C, D] :=: D[A | C, B & D]
  // D[A, B] & D[C, D] :=: D[A & C, B | D]
  for (const [schema, dts] of declaredTypesBySchema) {
    if (dts.length === 1) {
      result.push(dts[0])
    } else {
      const parameters = schema.typeParameters
      const typeArguments: Array<Type> = []

      for (let i = 0; i < parameters.length; i += 1) {
        const parameter = parameters[i]
        // Note that `typeArguments` will definitely be defined, because the schema has at least one parameter.
        const argumentParts = dts.map(dt => (<Array<Type>> dt.typeArguments)[i])
        switch (parameter.variance) {
          case Variance.Covariant:
            typeArguments.push(constructCovariant(argumentParts))
            break

          case Variance.Contravariant:
            typeArguments.push(constructContravariant(argumentParts))
            break

          case Variance.Invariant:
            throw Error(`At this stage, a type parameter may not be invariant. Type schema: ${schema.name}.`)
        }
      }

      // Note that this instantiation ignores a struct type's open property types. This is okay, as combined types
      // shouldn't contain open property types anyway.
      try {
        result.push(DeclaredSchemas.type(schema, typeArguments))
      } catch (e) {
        if (e instanceof InvalidTypeArguments) {
          // If the schema cannot be instantiated with the combined type arguments, default to the uncombined declared
          // types.
          result.push(...dts)
        } else {
          throw e
        }
      }
    }
  }

  return result
}

function filterRelevantTypes(parts: Array<Type>, kind: Kind): Array<Type> {
  switch (kind) {
    case Kind.Sum: return allExcluding(parts, (self, other) => isSubtype(self, other))
    case Kind.Intersection: return allExcluding(parts, (self, other) => isSubtype(other, self))
    default: throw invalidKind(kind)
  }
}

function invalidKind(kind: Kind): Error {
  return Error(`Cannot construct a sum or intersection type from kind ${kind}.`)
}
