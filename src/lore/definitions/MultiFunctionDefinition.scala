package lore.definitions

import lore.types.{Subtyping, Type}

case class MultiFunctionDefinition(name: String, functions: List[FunctionDefinition]) {
  /**
    * Returns the multi-function fit as defined in the specification.
    */
  def fit(t: Type): Set[FunctionDefinition] = {
    //functions.foreach(f => println(s"$t <= ${f.inputType}? ${Subtyping.isSubtype(t, f.inputType)}"))
    functions.filter(f => Subtyping.isSubtype(t.toTuple, f.inputType)).toSet
  }

  /**
    * Returns the function with the exact given input type.
    */
  def exact(inputType: Type): Option[FunctionDefinition] = {
    functions.find(f => f.inputType == inputType.toTuple)
  }
}
