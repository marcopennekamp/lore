package lore.compiler.semantics.functions

import lore.compiler.phases.transpilation.RuntimeNames
import lore.compiler.semantics.scopes.Variable
import lore.compiler.target.Target
import lore.compiler.types.ProductType

case class MultiFunctionDefinition(name: String, functions: Vector[FunctionDefinition]) extends Variable {

  val hierarchy: DispatchHierarchy = DispatchHierarchyBuilder.build(this)

  override val asTargetVariable: Target.Variable = RuntimeNames.multiFunction(this) // TODO: Do we need this?
  val runtimeName: Target.TargetName = asTargetVariable.name

  /**
    * Calculates the multi-function's fit set for the given type.
    */
  def fit(tpe: ProductType): Vector[FunctionDefinition] = Dispatch.fit(hierarchy, tpe)

  /**
    * Calculates the multi-function's min set for the given type.
    */
  def min(tpe: ProductType): Vector[FunctionDefinition] = Dispatch.min(hierarchy, tpe)

  /**
    * Returns the function with the exact given input type contained in this multi-function.
    */
  def exact(tpe: ProductType): Option[FunctionDefinition] = Dispatch.exact(hierarchy, tpe)

}
