import { Kind } from './types/kinds.ts'
import { typeOf } from './types/typeof.ts'
import { Type, XaryType } from './types/types.ts'
import { orderedHashWithSeed } from './utils/hash.ts'
import { Value } from './values.ts'

export interface TupleType extends XaryType { }

function createType(types: Array<Type>): TupleType {
  return { kind: Kind.Tuple, types, hash: orderedHashWithSeed(types, 0x4baf1ec8) }
}

const unitType: TupleType = createType([])

export interface TupleValue extends Value {
  elements: Array<any>
  lore$type: TupleType
}

export const Tuple = {
  type: createType,

  tupled(type: Type): TupleType {
    if (type.kind === Kind.Tuple) {
      return <TupleType> type
    } else {
      return createType([type])
    }
  },

  /**
   * Creates a tuple type WITHOUT a sensible hash. This should ONLY be used by the compiler to optimize
   * operations that don't require a hash, such as multiple dispatch resolution with a disabled cache.
   */
  unhashedType(types: Array<Type>): TupleType {
    return { kind: Kind.Tuple, types, hash: 0 }
  },

  unitType,

  value(elements: Array<any>): TupleValue {
    const elementTypes = new Array(elements.length)
    for (let i = 0; i < elements.length; i += 1) {
      elementTypes[i] = typeOf(elements[i])
    }
    return { elements, lore$type: Tuple.type(elementTypes) }
  },

  unitValue: { elements: [], lore$type: unitType } as TupleValue,

  get(tuple: TupleValue, index: number): any {
    // TODO: Surely proper bounds checking, no?
    return tuple.elements[index];
  },
}
