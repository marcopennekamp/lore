package lore.compiler.semantics.structures

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.VirtualMember
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.{DynamicCallTarget, ParameterDefinition}
import lore.compiler.syntax.ExprNode
import lore.compiler.types.Type

// TODO: "mutable" should actually be "writeable", since immutability implies that the whole data structure within
//       that property is unchangeable, while we are actually just gating the top-level write access to the property.
//       This is a subtle difference, but it should be honored. Perhaps we can later introduce a "deep" kind of
//       immutability which doesn't just make a property readonly, but actually applies to the whole data structure.
//       We can just say: `let var x = 5`

class PropertyDefinition(
  name: String,
  tpe: Type,
  isMutable: Boolean,
  defaultValueNode: Option[ExprNode],
  override val position: Position,
) extends Positioned {

  /**
    * This is a variable because it may be transformed during the course of the compilation.
    */
  var defaultValue: Option[DefaultValue] = _

  def hasDefault: Boolean = defaultValueNode.nonEmpty

  def asParameter: ParameterDefinition = ParameterDefinition(name, tpe, position)
  def asVirtualMember: VirtualMember = VirtualMember(name, tpe, isMutable = isMutable, underlying = Some(this))

}

case class DefaultValue(expression: Expression, callTarget: DynamicCallTarget)
