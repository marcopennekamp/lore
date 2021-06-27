import { Kind } from './types/kinds.ts'
import { typeOf } from './types/typeof.ts'
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

  name(symbol: SymbolValue): string {
    // See `pyramid/symbol.lore` on why this is necessary.
    if (typeOf(symbol).kind !== Kind.Symbol) {
      throw Error('The `Symbol.name` function must be invoked with a symbol value.')
    }
    return symbol.name
  },
}
