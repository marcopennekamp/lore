import { areEqual } from './equality.ts'
import { isSubtype } from './subtyping.ts'
import { fits, fitsMonomorphic, fitsPolymorphic } from './fit.ts'
import {
  any,
  boolean,
  struct,
  component,
  int,
  intersection,
  intersectionSimplified,
  list,
  map,
  nothing,
  product,
  real,
  string,
  sum,
  sumSimplified,
  unhashedProduct,
  unit,
  variable, structSchema,
} from './types.ts'
import { typeOf } from './typeof.ts'

export default {
  // Type constants.
  any,
  nothing,
  real,
  int,
  boolean,
  string,
  unit,

  // Type constructors.
  variable,
  sum,
  sumSimplified,
  intersection,
  intersectionSimplified,
  product,
  component,
  list,
  map,
  struct,
  //trait,

  // Type schemas.
  schema: {
    struct: structSchema,
    //trait: traitSchema,
  },

  // Unsafe constructors.
  unsafe: {
    unhashedProduct,
  },

  // Type relationships.
  isSubtype,
  areEqual,
  fits,
  fitsMonomorphic,
  fitsPolymorphic,
  typeOf,
}
