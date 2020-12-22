import { Type } from './types.ts'
import { SubtypingEnvironment } from "./SubtypingEnvironment.ts";

const defaultSubtyping = new SubtypingEnvironment()

/**
 * Checks whether t1 is a subtype of t2.
 */
export function isSubtype(t1: Type, t2: Type): boolean {
  return defaultSubtyping.isSubtype(t1, t2)
}
