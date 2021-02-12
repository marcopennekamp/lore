import { Struct, StructSchema, StructType, StructValue } from '../structs.ts'
import { TraitType } from '../traits.ts'
import { isSubtype } from './subtyping.ts'
import { typeOf } from './typeof.ts'
import { Type } from './types.ts'

interface IntrospectionType extends StructValue {
  type: Type
}

let introspectionTypeSchema: StructSchema;
let introspectionTypeType: StructType;

/**
 * The Introspection API allows Lore code to investigate its own types. The separate API (from the types API) is
 * necessary because we need to encapsulate the Lore type in a struct value that Lore can understand.
 */
export const Introspection = {
  /**
   * Initializes the Introspection API with the Type trait generated by the compiler. May only be called once by
   * the compiler. (Seriously, don't call into this with a dynamic call!!)
   */
  initialize(typeTrait: TraitType) {
    introspectionTypeSchema = Struct.schema("lore$introspectionType", [typeTrait], { })
    introspectionTypeType = Struct.type(introspectionTypeSchema, true)
  },

  typeOf(value: any): IntrospectionType {
    return Struct.value({ type: typeOf(value) }, introspectionTypeType) as IntrospectionType;
  },

  isSubtype(t1: IntrospectionType, t2: IntrospectionType): boolean {
    return isSubtype(t1.type, t2.type);
  }
}