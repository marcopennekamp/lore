package lore.functions

import lore.types.{Subtyping, Type}

case class MultiFunction(name: String, functions: Set[LoreFunction]) {

  def fit(t: Type): Set[LoreFunction] = {
    //functions.foreach(f => println(s"t <= ${f.inputType}? ${Subtyping.isSubtype(t, f.inputType)}"))
    functions.filter(f => Subtyping.isSubtype(t.toTuple, f.inputType))
  }

  /**
    * @return The function with the exact given input type.
    */
  def function(inputType: Type): Option[LoreFunction] = {
    functions.find(f => f.inputType == inputType.toTuple)
  }

}
