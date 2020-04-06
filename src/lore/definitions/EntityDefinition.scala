package lore.definitions

import lore.types.EntityType

class EntityDefinition(override val name: String, override val tpe: EntityType) extends DataTypeDefinition {
  override def supertypeDefinition: Option[DataTypeDefinition] = tpe.supertype.map(_.definition)
}
