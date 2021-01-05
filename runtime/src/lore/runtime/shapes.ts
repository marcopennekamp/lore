import { Intersection } from './intersections.ts'
import { Kind } from './types/kinds.ts'
import { PropertyTypes, Type } from './types/types.ts'
import { Hashed, pairHash, stringHash, unorderedHashWithSeed } from './utils/hash.ts'

export interface ShapeType extends Type {
  propertyTypes: PropertyTypes
}

export const Shape = {
  type(propertyTypes: PropertyTypes): ShapeType {
    // Note that creating the Hashable intermediate objects is only needed because unorderedHashWithSeed and pairHash
    // have the API limitation of expecting { hash: string } objects. If this ever causes performance problems, we can
    // move to a separate implementation that accepts raw hashes.
    const propertyHashes: Array<Hashed> = []
    for (const name of Object.keys(propertyTypes)) {
      let hash = pairHash({ hash: stringHash(name) }, propertyTypes[name], 0x4cb3052e)
      propertyHashes.push({ hash })
    }

    return { kind: Kind.Shape, propertyTypes, hash: unorderedHashWithSeed(propertyHashes, 0xf38da2c4) }
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
