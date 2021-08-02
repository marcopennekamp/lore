import { TraitType } from './traits.ts'
import {
  DeclaredType, DeclaredTypeSchema, hashPropertyTypes, LazyPropertyTypes, PropertyTypes,
} from './types/declared-types.ts'
import { Kind } from './types/kinds.ts'
import { Type } from './types/types.ts'
import { pairHashRaw, stringHash, stringHashWithSeed } from './utils/hash.ts'
import { Value } from './values.ts'

export interface StructSchema extends DeclaredTypeSchema {
  /**
   * The struct's properties, with each name as the key, and their respective compile-time types.
   *
   * Since open property types are handled by the struct's transpiled instantiation function, we do not need to
   * label properties as open at run-time. The specific type instance's property types will simply override the
   * property types found in the schema's map.
   *
   * The property type map must contain lazy types, because schemas are created at the start of the program with no
   * respect to property type order. Property types can easily reference declared types, which would lead to undefined
   * types if types are loaded in the wrong order. Since schema property types will be infrequently accessed, likely
   * only for structural typing, making the type lazy is acceptable.
   */
  propertyTypes: LazyPropertyTypes
}

// TODO: Interning struct types might bring big performance gains for multiple dispatch, because the more open properties a
//       struct has, the more expensive the areEqual test gets. This test HAS to be performed at least once per
//       dispatch cache access. Interned struct types would allow us to decide struct type equality via referential
//       equality as there would be exactly one struct type per property type combination. Many structs may only ever
//       have their archetype, too, which would further speed up type checking.

/**
 * The type of a struct. For each struct that is instantiated (assuming the struct has open properties), a new struct
 * type is created, as the list of open property types differs based on the actual properties present at run-time.
 */
export interface StructType extends DeclaredType {
  schema: StructSchema

  /**
   * The actual run-time types of the struct's properties IF they deviate from their compile-time type. The map may be
   * empty or undefined.
   */
  propertyTypes?: PropertyTypes
}

/**
 * A struct value consists of the lore$type property and all of its properties directly.
 */
export interface StructValue extends Value {
  // TODO: Rethink the lore$ naming scheme to bring it more in line with other transpiled names.
  lore$type: StructType
}

export const Struct = {
  schema(name: string, supertraits: Array<TraitType>, propertyTypes: LazyPropertyTypes): StructSchema {
    return { name, supertraits, propertyTypes }
  },

  type(schema: StructSchema, isArchetype: boolean, propertyTypes?: PropertyTypes): StructType {
    return {
      kind: Kind.Struct,
      schema,
      isArchetype,
      propertyTypes,
      hash: this.hash(schema, propertyTypes),
    }
  },

  hash(schema: StructSchema, propertyTypes?: PropertyTypes): number {
    if (!propertyTypes) {
      return stringHashWithSeed(schema.name, 0x38ba128e)
    }
    return pairHashRaw(stringHash(schema.name), hashPropertyTypes(propertyTypes, 0x281eba38), 0x38ba128e)
  },

  /**
   * Creates a new Lore object with the given properties and struct type. The 'properties' object will be used as
   * the final object value, so it should be seen as permanently borrowed.
   */
  value(properties: any, type: StructType): StructValue {
    const value = properties as StructValue
    value.lore$type = type
    return value
  },

  getPropertyType(type: StructType, name: string): Type {
    return (type.propertyTypes && type.propertyTypes[name]) ?? type.schema.propertyTypes[name]?.value()
  },
}
