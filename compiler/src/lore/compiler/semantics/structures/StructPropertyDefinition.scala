package lore.compiler.semantics.structures

import lore.compiler.core.{Position, Positioned}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.{CallTarget, ParameterDefinition}
import lore.compiler.semantics.members.Member
import lore.compiler.syntax.ExprNode
import lore.compiler.types.{Type, TypeVariable}

/**
  * The property of a struct.
  *
  * The position is restricted to the property's name for better error highlighting and index building.
  */
class StructPropertyDefinition(
  val name: String,
  val tpe: Type,
  val isOpen: Boolean,
  val isMutable: Boolean,
  val defaultValueNode: Option[ExprNode],
  override val position: Position,
) extends Positioned {

  /**
    * This is a variable because it may be transformed during the course of the compilation.
    */
  var defaultValue: Option[StructPropertyDefinition.DefaultValue] = _

  def hasDefault: Boolean = defaultValueNode.nonEmpty

  /**
    * Instantiates the property definition with type variables substituted using the given assignments.
    */
  def instantiate(assignments: TypeVariable.Assignments): StructPropertyDefinition.Instance = {
    StructPropertyDefinition.Instance(this, Type.substitute(tpe, assignments))
  }

}

object StructPropertyDefinition {
  case class DefaultValue(expression: Expression, callTarget: CallTarget.Dynamic) {
    val tpe: Type = expression.tpe
  }

  case class Instance(definition: StructPropertyDefinition, tpe: Type) {
    def asParameter: ParameterDefinition = ParameterDefinition(definition.name, tpe, definition.position)
    def asMember: Member = Member(definition.name, tpe, isAssignable = definition.isMutable, definition.isMutable)
  }
}
