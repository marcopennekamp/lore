package lore.functions

import lore.types.Type

case class MultiFunction(name: String, functions: Set[LoreFunction]) {

  def fit(tpe: Type): Set[LoreFunction] = {
    functions.filter(_.inputType.isSupertype(tpe))
  }

}
