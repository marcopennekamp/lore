package lore.compiler.semantics.variables

import lore.compiler.core.Position
import lore.compiler.semantics.bindings.TypedTermBinding
import lore.compiler.semantics.definitions.{BindingDefinitionKind, HasLocalModule, TermDefinition}
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.expressions.typed.Expression
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

  val value: Once[Expression] = new Once

  override def definitionKind: BindingDefinitionKind = BindingDefinitionKind.GlobalVariable
  override def position: Position = node.position

}
