import { Kind } from './types/kinds.ts'
import { Type } from './types/types.ts'
import { stringHashWithSeed} from './utils/hash.ts'
import { Value } from './values.ts'

export interface SymbolType extends Type {
  name: string
}

export interface SymbolValue extends Value {
  name: string
  lore$type: SymbolType
}

export const Symbol = {
  type(name: string): SymbolType {
    return { kind: Kind.Symbol, name, hash: stringHashWithSeed(name, 0xdc07cdd9) }
  },

  value(type: SymbolType): SymbolValue {
    return { name: type.name, lore$type: type }
  },
}
