package lore.compiler.semantics.functions

import lore.compiler.core.Compilation
import lore.compiler.feedback.Feedback
import lore.compiler.phases.transpilation.RuntimeNames
import lore.compiler.semantics.scopes.Variable
import lore.compiler.target.Target
import lore.compiler.types.TupleType

case class MultiFunctionDefinition(name: String, functions: Vector[FunctionDefinition]) extends Variable {

  val hierarchy: DispatchHierarchy = DispatchHierarchyBuilder.build(this)

  override val targetVariable: Target.Variable = RuntimeNames.multiFunction(this)

  /**
    * Resolves a multiple dispatch application of the multi-function for the given type. The empty fit and ambiguous
    * call errors must be customized.
    */
  def dispatch(
    tpe: TupleType,
    emptyFit: => Feedback.Error,
    ambiguousCall: Vector[FunctionDefinition] => Feedback.Error,
  ): Compilation[FunctionInstance] = {
    Dispatch.resolve(hierarchy, tpe, emptyFit, ambiguousCall)
  }

  /**
    * Calculates the multi-function's fit set for the given type.
    */
  def fit(tpe: TupleType): Vector[FunctionDefinition] = Dispatch.fit(hierarchy, tpe)

  /**
    * Calculates the multi-function's min set for the given type.
    */
  def min(tpe: TupleType): Vector[FunctionDefinition] = Dispatch.min(hierarchy, tpe)

}
