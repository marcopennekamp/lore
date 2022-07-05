package lore.compiler.semantics.structures

import lore.compiler.core.{Position, Positioned, UniqueKey}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.ParameterDefinition
import lore.compiler.semantics.members.Member
import lore.compiler.syntax.ExprNode
import lore.compiler.types.{StructSchema, Type, TypeVariable}

// TODO (multi-import): Rename to StructProperty and move to the `types` package.

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
  val struct: StructSchema,
  override val position: Position,
) extends Positioned {

  val uniqueKey: UniqueKey = UniqueKey.fresh()

  /**
    * This is a variable because it may be transformed during the course of the compilation.
    */
  var defaultValue: Option[Expression] = _

  def hasDefault: Boolean = defaultValueNode.nonEmpty

  /**
    * Instantiates the property definition with type variables substituted using the given assignments.
    */
  def instantiate(assignments: TypeVariable.Assignments): StructPropertyDefinition.Instance = {
    StructPropertyDefinition.Instance(this, Type.substitute(tpe, assignments))
  }

  override def toString: String = name

  override def equals(obj: Any): Boolean = obj match {
    case other: StructPropertyDefinition => uniqueKey == other.uniqueKey
  }

  override val hashCode: Int = uniqueKey.hashCode()

}

object StructPropertyDefinition {
  case class Instance(definition: StructPropertyDefinition, tpe: Type) {
    def asParameter: ParameterDefinition = ParameterDefinition(definition.uniqueKey, Some(definition.name), tpe, definition.position)
    def asMember: Member = Member(definition.name, tpe, isAssignable = definition.isMutable, definition.isMutable)
  }
}
