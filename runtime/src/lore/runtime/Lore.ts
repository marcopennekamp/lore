import { Intersection } from './intersections.ts'
import { LoreIO } from './io.ts'
import { List } from './lists.ts'
import { Map } from './maps.ts'
import { LoreMath } from './math.ts'
import { Numbers } from './numbers.ts'
import { Shape } from './shapes.ts'
import { StringFunctions } from './strings.ts'
import { Struct } from './structs.ts'
import { Sum } from './sums.ts'
import { Symbol } from './symbols.ts'
import { Trait } from './traits.ts'
import { Tuple } from './tuples.ts'
import { Types } from './types/types.ts'
import utils from './utils/api.ts'
import { Values } from './values.ts'
import { Function } from './functions.ts'

export default {
  // APIs used by the compiler.
  types: Types,
  values: Values,
  sums: Sum,
  intersections: Intersection,
  tuples: Tuple,
  functions: Function,
  lists: List,
  maps: Map,
  shapes: Shape,
  strings: StringFunctions,
  symbols: Symbol,
  traits: Trait,
  structs: Struct,
  utils,

  // APIs used by Pyramid.
  io: LoreIO,
  math: LoreMath,
  numbers: Numbers,
}
