package lore.compiler.semantics.variables

import lore.compiler.core.Position
import lore.compiler.semantics.bindings.TypedTermBinding
import lore.compiler.semantics.definitions.{HasLocalModule, TermDefinition}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.{BindingKind, NamePath}
import lore.compiler.syntax.DeclNode.GlobalVariableNode
import lore.compiler.types.Type
import lore.compiler.utils.Once

class GlobalVariableDefinition(
  override val name: NamePath,
  val node: GlobalVariableNode,
) extends TermDefinition with TypedTermBinding with HasLocalModule {

  private val _tpe: Once[Type] = new Once

  override def tpe: Type = _tpe

  def initialize(tpe: Type): Unit = {
    _tpe.assign(tpe)
  }

  override def isInitialized: Boolean = _tpe.isAssigned

  /**
    * The global variable's `value` expression is attached during the transformation phase.
    */
  var value: Expression = _

  override def bindingKind: BindingKind = BindingKind.GlobalVariable
  override def position: Position = node.position

}
