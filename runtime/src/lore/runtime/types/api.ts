import { areEqual } from './equality.ts'
import { isSubtype } from './subtyping.ts'
import { fits, fitsMonomorphic, fitsPolymorphic } from './fit.ts'
import {
  any,
  boolean,
  classType,
  component,
  int,
  intersection,
  list,
  map,
  nothing,
  product,
  real,
  string,
  sum,
  unhashedProduct,
  unit,
  variable,
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
  intersection,
  sum,
  product,
  component,
  list,
  map,
  classType,

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
