import { Function, FunctionValue } from './functions.ts'
import { TraitType } from './traits.ts'
import { Tuple, TupleType } from './tuples.ts'
import { DeclaredType, DeclaredTypes } from './types/declared-types.ts'
import { Kind } from './types/kinds.ts'
import { DeclaredSchemas, DeclaredSchema } from './types/declared-schemas.ts'
import { LazyPropertyTypes, PropertyTypes } from './types/property-types.ts'
import { substitute } from './types/substitution.ts'
import { Assignments, TypeVariable } from './types/type-variables.ts'
import { Type } from './types/types.ts'
import { Value } from './values.ts'

export interface StructSchema extends DeclaredSchema {
  /**
   * The struct's properties, with each name as the key, and their respective compile-time types.
   *
   * Since open property types are handled by the struct's transpiled instantiation function, we do not need to
   * label properties as open at run-time. The specific struct type's open property types will simply override the
   * property types found in the schema's map.
   *
   * The property type map must contain lazy types, because schemas are initialized at the start of the program with no
   * respect to property type order. Property types can easily reference declared types, which would lead to undefined
   * types if types are initialized in the wrong order. Since schema property types will be infrequently accessed,
   * likely only for structural subtyping, making the type lazy is acceptable.
   */
  propertyTypes: LazyPropertyTypes

  /**
   * The struct's property names in their order of declaration. We keep this extra bit of information because
   * `propertyTypes` does not remember the order of properties.
   */
  propertyOrder: Array<string>

  /**
   * The struct's open property names in their order of declaration. This is used to create a type's unique key with
   * open properties in the correct order.
   */
  openPropertyOrder: Array<string>
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
   * The actual run-time types of the struct's open properties IF they deviate from their compile-time type. If there
   * are no such deviations, the map **must** be `undefined`.
   *
   * This map does not contain instantiated versions of the schema's non-open properties.
   */
  openPropertyTypes?: PropertyTypes

  /**
   * Caches the result of `getConstructor`.
   */
  constructorCache?: FunctionValue<StructValue>

  /**
   * Caches the result of `getPropertyType` for non-open properties of parameterized structs.
   */
  propertyTypeCache?: PropertyTypes
}

/**
 * A struct value consists of the lore$type property and all of its properties directly.
 */
export interface StructValue extends Value {
  // TODO: Rethink the lore$ naming scheme to bring it more in line with other transpiled names.
  lore$type: StructType
}

export const Struct = {
  schema(
    name: string,
    typeParameters: Array<TypeVariable>,
    supertraits: Array<TraitType>,
    hasMultipleParameterizedInheritance: boolean,
    propertyTypes: LazyPropertyTypes,
    propertyOrder: Array<string>,
    openPropertyOrder: Array<string>,
  ): StructSchema {
    return DeclaredSchemas.schema<StructSchema>(
      Kind.Struct,
      name,
      typeParameters,
      supertraits,
      hasMultipleParameterizedInheritance,
      (schema, typeArguments) => Struct.type(schema, typeArguments, undefined),
      { propertyTypes, propertyOrder, openPropertyOrder },
    )
  },

  /**
   * Instantiates a new struct type from the given type arguments and open property types.
   */
  type(schema: StructSchema, typeArguments?: Assignments, openPropertyTypes?: PropertyTypes): StructType {
    let uniqueKey: TupleType | undefined = undefined
    if (openPropertyTypes) {
      // To generate the struct type's unique key, we must add open properties in their order of declaration.
      const uniqueKeyTypes = typeArguments ? typeArguments.slice() : []
      for (const propertyName of schema.openPropertyOrder) {
        uniqueKeyTypes.push(openPropertyTypes[propertyName])
      }
      uniqueKey = Tuple.type(uniqueKeyTypes)
    } else if (typeArguments) {
      uniqueKey = Tuple.type(typeArguments)
    }

    return DeclaredTypes.type<StructType>(Kind.Struct, schema, typeArguments, { openPropertyTypes }, uniqueKey)
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

  /**
   * Gets the constructor function for the given schema and type arguments.
   *
   * The compiler will only utilize this function if the struct has type parameters. Otherwise, a constant constructor
   * will be generated for the whole schema. It is also only used in places where the function value is further passed
   * around. Immediate invocation of a call-style constructor is optimized by the compiler to a simple `construct`
   * call. These considerations combined, there should be no large performance hit considering that the function is
   * called infrequently. The constructor is cached for memory footprint reasons, similar to why struct types are
   * interned.
   *
   * @param construct The `construct` function as generated by the compiler. Note that the first argument of
   *                  `construct` for parameterized structs is the list of type arguments.
   */
  getConstructor(
    schema: StructSchema,
    typeArguments: Assignments,
    construct: (typeArguments: Assignments, ...args: any) => StructValue,
  ): FunctionValue<StructValue> {
    const structType = Struct.type(schema, typeArguments)
    if (structType.constructorCache) {
      return structType.constructorCache
    }

    const parameterTypes: Array<Type> = []
    for (const name of schema.propertyOrder) {
      parameterTypes.push(<Type> Struct.getPropertyType(structType, name))
    }

    return Function.value(
      construct.bind(null, typeArguments),
      Function.type(Tuple.type(parameterTypes), structType)
    )
  },

  /**
   * Get the struct type's property type, which is either an open property type, or the schema's property type
   * instantiated with the struct's type arguments. This result is cached for non-open properties of parametric structs
   * to avoid having to substitute type arguments into the schema's property types.
   */
  getPropertyType(type: StructType, name: string): Type | undefined {
    const openCandidate = type.openPropertyTypes && type.openPropertyTypes[name]
    if (openCandidate) {
      return openCandidate
    }

    const schemaPropertyType = type.schema.propertyTypes[name]?.value()
    if (!schemaPropertyType) {
      return undefined
    }

    if (!type.typeArguments) {
      return schemaPropertyType
    }

    if (!type.propertyTypeCache) {
      type.propertyTypeCache = { }
    }

    const cachedCandidate = type.propertyTypeCache[name]
    if (cachedCandidate) {
      return cachedCandidate
    }

    const propertyType = substitute(type.typeArguments, schemaPropertyType)
    type.propertyTypeCache[name] = propertyType
    return propertyType
  },
}
