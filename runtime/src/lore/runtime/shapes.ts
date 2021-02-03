import { Intersection } from './intersections.ts'
import { Kind } from './types/kinds.ts'
import { hashPropertyTypes, PropertyTypes, Type } from './types/types.ts'

export interface ShapeType extends Type {
  propertyTypes: PropertyTypes
}

export const Shape = {
  type(propertyTypes: PropertyTypes): ShapeType {
    return { kind: Kind.Shape, propertyTypes, hash: hashPropertyTypes(propertyTypes, 0xf38da2c4) }
  },

  /**
   * Combines the given shape types into a single shape type. Mirrors the compiler implementation of ShapeType.combine.
   *
   * This operation is expensive because for each property with more than one occurrence, we need to simplify the
   * intersection of the property's types.
   */
  combine(shapes: Array<ShapeType>): ShapeType {
    const combinedProperties: Map<string, Array<Type>> = new Map()
    for (const shape of shapes) {
      for (const propertyName of Object.keys(shape.propertyTypes)) {
        const propertyType = shape.propertyTypes[propertyName]
        const list = combinedProperties.get(propertyName)
        if (!list) {
          combinedProperties.set(propertyName, [propertyType])
        } else {
          list.push(propertyType)
        }
      }
    }

    const shapeProperties: { [key: string]: Type } = { }
    for (const [name, types] of combinedProperties) {
      shapeProperties[name] = Intersection.simplified(types)
    }

    return Shape.type(shapeProperties)
  },
}
