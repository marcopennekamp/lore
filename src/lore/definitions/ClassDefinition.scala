package lore.definitions

import lore.types.ClassType

class ClassDefinition(override val name: String, override val tpe: ClassType, override val properties: List[PropertyDefinition]) extends DataTypeDefinition {
  override def supertypeDefinition: Option[ClassDefinition] = tpe.supertype.map(_.definition)
}
