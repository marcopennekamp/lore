package lore.compiler.types

import lore.compiler.semantics.structures.LabelDefinition

// TODO: Do we even WANT label types to be in a hierarchical relationship to each other?
class LabelType(
  override val name: String, override val supertype: Option[LabelType],
) extends DeclaredType with DeclaredType.DefinitionProperty[LabelDefinition] {
  override def rootSupertype: LabelType = super.rootSupertype.asInstanceOf[LabelType]
}
