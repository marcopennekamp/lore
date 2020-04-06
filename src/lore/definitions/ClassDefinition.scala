package lore.definitions

import lore.types.ClassType

/**
  * @param localProperties The properties declared within this class. Does not include supertype properties.
  */
class ClassDefinition(override val name: String, override val tpe: ClassType, val localProperties: List[PropertyDefinition]) extends DataTypeDefinition {
  override def supertypeDefinition: Option[ClassDefinition] = tpe.supertype.map(_.definition)
  override def properties: List[PropertyDefinition] = supertypeDefinition.map(_.properties).getOrElse(List.empty) ++ localProperties
  override def members: List[MemberDefinition] = properties
}
