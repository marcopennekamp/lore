package lore.definitions

import lore.compiler.C
import lore.types.{ClassType, ComponentType, Type}

/**
  * A data type member, that is, either a property or a component.
  */
sealed trait MemberDefinition[+T <: Type] extends TypingDeferred[T] {
  def name: String
  def isMutable: Boolean = false
}

class PropertyDefinition(
  override val name: String, override val resolveType: () => C[Type], override val isMutable: Boolean
) extends MemberDefinition[Type]

/**
  * @param name The name of the component (and at the same time its type name).
  * @param overrides The component name of the superclass that this component overrides.
  */
class ComponentDefinition(
  override val name: String, override val resolveType: () => C[ClassType], val overrides: Option[String]
) extends MemberDefinition[ClassType] {
  val componentType: ComponentType = ComponentType(tpe)
}
