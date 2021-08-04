package lore.compiler.semantics.functions

import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.scopes.Binding
import lore.compiler.types.TupleType

case class MultiFunctionDefinition(name: String, functions: Vector[FunctionDefinition]) extends Binding {

  val hierarchy: DispatchHierarchy = DispatchHierarchyBuilder.build(this)

  /**
    * Resolves a multiple dispatch application of the multi-function for the given type. The empty fit and ambiguous
    * call errors must be customized.
    */
  def dispatch(
    tpe: TupleType,
    emptyFit: => Feedback.Error,
    ambiguousCall: Vector[FunctionDefinition] => Feedback.Error,
  )(implicit reporter: Reporter): Option[FunctionInstance] = {
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
