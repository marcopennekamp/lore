package lore.compiler.semantics.variables

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.scopes.TypedBinding
import lore.compiler.syntax.ExprNode
import lore.compiler.types.Type

class GlobalVariableDefinition(
  override val name: String,
  override val tpe: Type,
  val valueNode: ExprNode,
  override val position: Position,
) extends TypedBinding with Positioned {

  /**
    * This is a variable because it may be transformed during the course of the compilation.
    */
  var value: Expression = _

}
