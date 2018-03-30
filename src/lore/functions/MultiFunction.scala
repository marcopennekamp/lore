package lore.functions

import lore.types.Type

case class MultiFunction(name: String, functions: Set[LoreFunction]) {

  def fit(tpe: Type): Set[LoreFunction] = {
    //functions.foreach(f => println(s"$tpe <= ${f.inputType}? ${f.inputType.isSupertype(tpe)}"))
    functions.filter(_.inputType.isSupertype(tpe))
  }

}
