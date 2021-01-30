package lore.compiler.semantics.structures

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.{CallTarget, ParameterDefinition}
import lore.compiler.semantics.members.Member
import lore.compiler.syntax.ExprNode
import lore.compiler.types.Type

class StructPropertyDefinition(
  val name: String,
  val tpe: Type,
  val isMutable: Boolean,
  val defaultValueNode: Option[ExprNode],
  override val position: Position,
) extends Positioned {

  /**
    * This is a variable because it may be transformed during the course of the compilation.
    */
  var defaultValue: Option[StructPropertyDefinition.DefaultValue] = _

  def hasDefault: Boolean = defaultValueNode.nonEmpty

  def asParameter: ParameterDefinition = ParameterDefinition(name, tpe, position)
  def asMember: Member = Member(name, tpe, isAssignable = isMutable, isMutable)

}

object StructPropertyDefinition {
  case class DefaultValue(expression: Expression, callTarget: CallTarget.Dynamic)
}
