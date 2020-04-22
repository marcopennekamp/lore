package lore.types

import lore.definitions.LabelDefinition

// TODO: Do we even WANT label types to be in a hierarchical relationship to each other?
class LabelType(override val supertype: Option[LabelType]) extends DeclaredType with DeclaredType.DefinitionProperty[LabelDefinition] {
  /**
    * A label type is abstract unless it is an augmentation. That case is handled in the implementation of
    * intersection type's isAbstract.
    */
  override def isAbstract = true

  override def verbose = s"$toString < $supertype"
}
