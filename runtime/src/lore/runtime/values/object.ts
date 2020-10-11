import { DeclaredType, StructType } from '../types/types.ts'
import { LoreValue } from './values.ts'
import utils from '../utils/api.ts'
import { isSubtype } from '../types/subtyping.ts'

/**
 * An object consists of the lore$type property and all of its properties directly. Components are contained in their
 * own object to make retrieval possible. Without this tiered structure, we'd have to tag each component member
 * individually as a component.
 */
export interface ObjectValue extends LoreValue {
  // TODO: Rethink the lore$ naming scheme to bring it more in line with other transpiled names.
  lore$type: StructType
  lore$components: any
}

export const api = {
  /**
   * Creates a new Lore object with the given properties, components and struct type. The 'properties' object will be
   * used as the final object value, so it should be seen as permanently borrowed.
   */
  create(properties: any, components: any | undefined, type: StructType): ObjectValue {
    const value = properties as ObjectValue
    if (components) {
      value.lore$components = components
    }
    value.lore$type = type
    return value
  },

  /**
   * Retrieves the component corresponding to the given declared type from the struct value. The actual component must
   * be a subtype of the declared type. For example:
   *
   *    - We have an entity with a compile-time component Animal. We access the component using entity.Animal. The
   *      struct value at run-time is actually Husky, which is a subtype of Animal. So given the trait Animal, we have
   *      to find the component with the type Husky. Incidentally, the member's name is Animal.
   *    - We have the same entity type with a Husky (declared Animal) component. This time, we view the entity through
   *      a variable entity: +Dog. This is legal because the entity has been dispatched based on its actual component
   *      type, so the component is at least a Dog. We can access it using entity.Dog, and so the target type given to
   *      this function will be Dog. We have to find the Husky component given the Dog type.
   *
   * The names found by this algorithm are cached in the struct schema's component access cache. We can cache the name
   * in the schema, instead of the type, because for a given declared type, the Lore compiler guarantees that the
   * member name will always stay the same, regardless of the actual types of each component.
   */
  retrieve(struct: ObjectValue, searchTarget: DeclaredType) {
    if (!searchTarget.schema.isOwnable) {
      throw Error(`Components may only be retrieved through ownable declared types. ${searchTarget.schema.name} is independent.`)
    }

    const schema = struct.lore$type.schema
    const components = struct.lore$components
    if (!schema.componentAccessCache) {
      schema.componentAccessCache = utils.typeMap.create()
    }

    const cachedName = schema.componentAccessCache.get(searchTarget)
    if (cachedName) {
      return components[cachedName]
    } else {
      const componentDefinitions = schema.components
      for (let i = 0; i < componentDefinitions.length; i += 1) {
        const name = componentDefinitions[i].name
        const component: ObjectValue = components[name]
        if (isSubtype(component.lore$type, searchTarget)) {
          schema.componentAccessCache.set(searchTarget, name)
          return component
        }
      }
      return undefined
    }
  },
}
