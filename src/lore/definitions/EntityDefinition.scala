package lore.definitions

import lore.types.EntityType

/**
  * @param localMembers The members declared within this entity. Does not include supertype members.
  */
class EntityDefinition(
  override val name: String,
  override val tpe: EntityType,
  val localMembers: List[MemberDefinition]
) extends DataTypeDefinition {
  override def supertypeDefinition: Option[DataTypeDefinition] = tpe.supertype.map(_.definition)
  override lazy val members: List[MemberDefinition] = supertypeDefinition.map(_.members).getOrElse(List.empty) ++ localMembers
  override def properties: List[PropertyDefinition] = members.flatMap { case property: PropertyDefinition => Some(property); case _ => None }
  def components: List[ComponentDefinition] = members.flatMap { case component: ComponentDefinition => Some(component); case _ => None }
}
