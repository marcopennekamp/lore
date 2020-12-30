package lore.compiler.semantics.structures

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.{DynamicCallTarget, ParameterDefinition}
import lore.compiler.semantics.members
import lore.compiler.semantics.members.Member
import lore.compiler.syntax.ExprNode
import lore.compiler.types.Type

// TODO: "mutable" should actually be "writeable", since immutability implies that the whole data structure within
//       that property is unchangeable, while we are actually just gating the top-level write access to the property.
//       This is a subtle difference, but it should be honored. Perhaps we can later introduce a "deep" kind of
//       immutability which doesn't just make a property readonly, but actually applies to the whole data structure.
//       We can just say: `let var x = 5`

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
  case class DefaultValue(expression: Expression, callTarget: DynamicCallTarget)
}