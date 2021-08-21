import { ShapeType } from '../shapes.ts'
import { Struct, StructType } from '../structs.ts'
import { TraitSchema } from '../traits.ts'
import { DeclaredType, DeclaredTypes } from './declared-types.ts'
import { Kind } from './kinds.ts'
import { Type } from './types.ts'

/**
 * The TypePaths namespace contains functions for type path steps which need to be resolved at run time.
 */
export const TypePaths = {
  shapeProperty(type: StructType | ShapeType, name: string): Type | undefined {
    if (type.kind === Kind.Struct) {
      return Struct.getPropertyType(<StructType> type, name)
    } else if (type.kind === Kind.Shape) {
      return (<ShapeType> type).propertyTypes[name]
    }
    return undefined
  },

  typeArgument(type: DeclaredType, schema: TraitSchema, index: number): Type | undefined {
    return DeclaredTypes.findSupertrait(type, schema)?.typeArguments?.[index]
  },
}
