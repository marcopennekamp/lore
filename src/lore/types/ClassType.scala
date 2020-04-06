package lore.types

import lore.definitions.ClassDefinition

class ClassType(
  override val supertype: Option[ClassType],
  override val isAbstract: Boolean
) extends DataType with DeclaredType.DefinitionProperty[ClassDefinition] {
  override def kindName: String = "class"
}
