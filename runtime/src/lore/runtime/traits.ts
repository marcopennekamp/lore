import { ShapeType } from './shapes.ts'
import { Tuple } from './tuples.ts'
import { DeclaredType, DeclaredTypes } from './types/declared-types.ts'
import { Kind } from './types/kinds.ts'
import { DeclaredSchemas, DeclaredTypeSchema } from './types/declared-schemas.ts'
import { substitute } from './types/substitution.ts'
import { Assignments, TypeVariable } from './types/type-variables.ts'
import { LazyValue } from './utils/LazyValue.ts'

export interface TraitSchema extends DeclaredTypeSchema {
  /**
   * As a trait may inherit directly and indirectly from shapes, each trait has an inherited shape type. This shape
   * type can be used to decide trait/shape subtyping at run-time. Since most values on the left side of subtyping
   * will actually be structs, the inherited shape type shouldn't be requested often, if at all.
   */
  inheritedShapeType: LazyValue<ShapeType>
}

/**
 * A trait type. Only one type is instantiated for each trait, as their supertypes are not dependent on run-time
 * values. Thus, a trait type is also always an archetype.
 */
export interface TraitType extends DeclaredType {
  schema: TraitSchema

  /**
   * The schema's inherited shape type instantiated with the trait's type arguments. This is only populated on demand
   * and only when the trait actually has type arguments.
   */
  inheritedShapeTypeCache?: ShapeType
}

export const Trait = {
  schema(
    name: string,
    typeParameters: Array<TypeVariable>,
    supertraits: Array<TraitType>,
    hasMultipleParameterizedInheritance: boolean,
    inheritedShapeType: LazyValue<ShapeType>,
  ): TraitSchema {
    return DeclaredSchemas.schema<TraitSchema>(name, typeParameters, supertraits, hasMultipleParameterizedInheritance, Trait.type, { inheritedShapeType })
  },

  type(schema: TraitSchema, typeArguments?: Assignments): TraitType {
    return DeclaredTypes.type<TraitType>(Kind.Trait, schema, typeArguments, { }, typeArguments ? Tuple.type(typeArguments) : undefined)
  },

  getInheritedShapeType(type: TraitType): ShapeType {
    if (!type.typeArguments) {
      return type.schema.inheritedShapeType.value()
    }

    if (type.inheritedShapeTypeCache) {
      return type.inheritedShapeTypeCache
    }

    const shapeType = <ShapeType> substitute(type.typeArguments, type.schema.inheritedShapeType.value())
    type.inheritedShapeTypeCache = shapeType
    return shapeType
  },
}
