package lore.functions

import lore.types.{Subtyping, Type}

case class MultiFunction(name: String, functions: Set[LoreFunction]) {

  def fit(t: Type): Set[LoreFunction] = {
    //functions.foreach(f => println(s"t <= ${f.inputType}? ${Subtyping.isSubtype(t, f.inputType)}"))
    functions.filter(f => Subtyping.isSubtype(t, f.inputType))
  }

}
