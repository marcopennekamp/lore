package lore.compiler.semantics.functions

import lore.compiler.core.Position
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.definitions.TermDefinition
import lore.compiler.semantics.{BindingKind, NamePath}
import lore.compiler.syntax.DeclNode.FunctionNode
import lore.compiler.types.TupleType
import lore.compiler.utils.Once

class MultiFunctionDefinition(
  override val name: NamePath,
) extends TermDefinition {

  private var _functionNodes: Vector[FunctionNode] = Vector.empty
  private val _functions: Once[Vector[FunctionDefinition]] = new Once
  private val _hierarchy: Once[DispatchHierarchy] = new Once

  /**
    * A list of collected function nodes belonging to the multi-function. This list is cleared after the multi-function
    * has been initialized.
    */
  def functionNodes: Vector[FunctionNode] = _functionNodes

  def functions: Vector[FunctionDefinition] = _functions
  def hierarchy: DispatchHierarchy = _hierarchy

  def addFunctionNode(node: FunctionNode): Unit = {
    _functionNodes :+= node
  }

  def initialize(functions: Vector[FunctionDefinition]): Unit = {
    _functions.assign(functions)
    _hierarchy.assign(DispatchHierarchyBuilder.build(this))
    _functionNodes = Vector.empty
  }

  override def isInitialized: Boolean = _functions.isAssigned && _hierarchy.isAssigned

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

  def positions: Vector[Position] = functions.map(_.position)
  override def position: Position = functions.headOption.map(_.position).getOrElse(Position.unknown)

  override def bindingKind: BindingKind = BindingKind.MultiFunction
  override def toString: String = name.toString

}

object MultiFunctionDefinition {
  def apply(name: NamePath, firstNode: FunctionNode): MultiFunctionDefinition = {
    val mf = new MultiFunctionDefinition(name)
    mf.addFunctionNode(firstNode)
    mf
  }
}
