import { areEqual } from './equality.ts'
import { isSubtype } from './subtyping.ts'
import { fits } from './fit.ts'
import {
  any,
  boolean,
  component,
  int, intersection,
  list,
  map,
  nothing,
  product,
  real,
  string,
  sum,
  unit,
  variable
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

  // Type relationships.
  isSubtype,
  areEqual,
  fits,
  typeOf,
}
