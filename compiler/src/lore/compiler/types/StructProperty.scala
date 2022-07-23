package lore.compiler.types

import lore.compiler.core.{Position, Positioned, UniqueKey}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.ParameterDefinition
import lore.compiler.semantics.members.Member
import lore.compiler.syntax.ExprNode
import lore.compiler.utils.Once

/**
  * The property of a struct.
  *
  * The position is restricted to the property's name for better error highlighting and index building.
  */
class StructProperty(
  val name: String,
  val tpe: Type,
  val isOpen: Boolean,
  val isMutable: Boolean,
  val defaultValueNode: Option[ExprNode],
  val struct: StructSchema,
  override val position: Position,
) extends Positioned {

  val uniqueKey: UniqueKey = UniqueKey.fresh()

  val defaultValue: Once[Option[Expression]] = new Once

  def hasDefault: Boolean = defaultValueNode.nonEmpty

  /**
    * Instantiates the property with type variables substituted using the given assignments.
    */
  def instantiate(assignments: TypeVariable.Assignments): StructProperty.Instance = {
    StructProperty.Instance(this, Type.substitute(tpe, assignments))
  }

  override def toString: String = name

  override def equals(obj: Any): Boolean = obj match {
    case other: StructProperty => uniqueKey == other.uniqueKey
  }

  override val hashCode: Int = uniqueKey.hashCode()

}

object StructProperty {
  case class Instance(property: StructProperty, tpe: Type) {
    def asParameter: ParameterDefinition = ParameterDefinition(
      property.uniqueKey,
      Some(property.name),
      tpe,
      property.position,
    )

    def asMember: Member = Member(
      property.name,
      tpe,
      isAssignable = property.isMutable,
      property.isMutable,
    )
  }
}
