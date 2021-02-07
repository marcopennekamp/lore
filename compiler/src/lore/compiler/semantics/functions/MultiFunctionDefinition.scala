package lore.compiler.semantics.functions

import lore.compiler.types.Type

case class MultiFunctionDefinition(name: String, functions: Vector[FunctionDefinition]) {

  val hierarchy: DispatchHierarchy = DispatchHierarchyBuilder.build(this)

  /**
    * Calculates the multi-function's fit set for the given type.
    */
  def fit(tpe: Type): Vector[FunctionDefinition] = Dispatch.fit(hierarchy, tpe)

  /**
    * Calculates the multi-function's min set for the given type.
    */
  def min(tpe: Type): Vector[FunctionDefinition] = Dispatch.min(hierarchy, tpe)

  /**
    * Returns the function with the exact given input type contained in this multi-function.
    */
  def exact(tpe: Type): Option[FunctionDefinition] = Dispatch.exact(hierarchy, tpe)

}
