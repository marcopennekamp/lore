package lore

import lore.types.Subtyping

package object functions {
  implicit class FunctionSetExtension(functions: Set[LoreFunction]) {
    def multiMin: Set[LoreFunction] = {
      functions.filter(f => !functions.filter(f2 => f2 != f).exists(f2 => Subtyping.isSubtype(f2.inputType, f.inputType)))
    }
  }
}
