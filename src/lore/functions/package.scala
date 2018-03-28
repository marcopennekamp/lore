package lore

package object functions {
  implicit class FunctionSetExtension(functions: Set[LoreFunction]) {
    def multiMin: Set[LoreFunction] = {
      functions.filter(f => !functions.filter(f2 => f2 != f).exists(f2 => f2.inputType.isSubtype(f.inputType)))
    }
  }
}
