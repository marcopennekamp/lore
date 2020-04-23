package lore.definitions

import lore.compiler.Compilation.C
import lore.compiler.feedback.Position
import lore.types.{ClassType, ComponentType, Type, TypingDeferred}

/**
  * A data type member, that is, either a property or a component.
  */
sealed trait MemberDefinition[+T <: Type] extends PositionedDefinition with TypingDeferred[T] {
  def name: String
  def isMutable: Boolean = false
}

class PropertyDefinition(
  override val name: String, override val typeResolver: () => C[Type], override val isMutable: Boolean,
  override val position: Position,
) extends MemberDefinition[Type]

/**
  * @param name The name of the component (and at the same time its type name).
  * @param overrides The component name of the superclass that this component overrides.
  */
class ComponentDefinition(
  override val name: String, override val typeResolver: () => C[ClassType], val overrides: Option[String],
  override val position: Position,
) extends MemberDefinition[ClassType] {
  lazy val componentType: ComponentType = ComponentType(tpe)
}
