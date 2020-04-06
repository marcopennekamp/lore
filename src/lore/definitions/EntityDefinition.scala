package lore.definitions

import lore.types.EntityType

class EntityDefinition(override val name: String, override val tpe: EntityType, val members: List[MemberDefinition]) extends DataTypeDefinition {
  override def supertypeDefinition: Option[DataTypeDefinition] = tpe.supertype.map(_.definition)
  override def properties: List[PropertyDefinition] = members.flatMap { case property: PropertyDefinition => Some(property); case _ => None }
  def components: List[ComponentDefinition] = members.flatMap { case component: ComponentDefinition => Some(component); case _ => None }
}
