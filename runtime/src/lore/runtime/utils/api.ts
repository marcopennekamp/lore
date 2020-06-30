import { TinyMap } from './TinyMap.ts'
import { TinySet } from './TinySet.ts'

export default {
  tinyMap: {
    get: TinyMap.get,
    add: TinyMap.set,
  },
  tinySet: {
    has: TinySet.has,
    add: TinySet.add,
  },
}
