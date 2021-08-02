import { Intersection } from './intersections.ts'
import { hashPropertyTypes, PropertyTypes } from './types/declared-types.ts'
import { Kind } from './types/kinds.ts'
import { typeOf } from './types/typeof.ts'
import { Type } from './types/types.ts'
import { Value } from './values.ts'

export interface ShapeType extends Type {
  propertyTypes: PropertyTypes
}

export interface ShapeValue extends Value {
  lore$type: ShapeType
}

export const Shape = {
  type(propertyTypes: PropertyTypes): ShapeType {
    return { kind: Kind.Shape, propertyTypes, hash: hashPropertyTypes(propertyTypes, 0xf38da2c4) }
  },

  /**
   * Creates a shape value from the given properties. The property types are determined at run-time using typeOf.
   */
  value(properties: any): ShapeValue {
    const propertyTypes: PropertyTypes = { }
    for (const key of Object.keys(properties)) {
      propertyTypes[key] = typeOf(properties[key])
    }

    const value = properties as ShapeValue
    value.lore$type = Shape.type(propertyTypes)
    return value
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
