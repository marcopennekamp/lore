package lore.types

import lore.definitions.EntityDefinition

class EntityType(override val supertype: Option[DataType], override val isAbstract: Boolean) extends DataType with DeclaredType.DefinitionProperty[EntityDefinition] {
  override def kindName: String = "entity"
}
