package lore.definitions

import lore.types.{ComponentType, DataType, Type}

/**
  * A data type member, that is, either a property or a component.
  */
sealed trait MemberDefinition {
  def name: String
  def tpe: Type
  def isMutable: Boolean = false
}

case class PropertyDefinition(name: String, tpe: Type, override val isMutable: Boolean) extends MemberDefinition
case class ComponentDefinition(name: String, tpe: DataType) extends MemberDefinition {
  val componentType: ComponentType = ComponentType(tpe)
}
