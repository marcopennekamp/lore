package lore.compiler.semantics.variables

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.semantics.scopes.TypedBinding
import lore.compiler.syntax.ExprNode
import lore.compiler.types.Type

class GlobalVariableDefinition(
  val name: NamePath,
  override val tpe: Type,
  val valueNode: ExprNode,
  val localModule: LocalModule,
  override val position: Position,
) extends TypedBinding with Positioned {

  /**
    * This is a variable because it may be transformed during the course of the compilation.
    */
  var value: Expression = _

}
