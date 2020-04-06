package lore.types

import lore.definitions.LabelDefinition
import lore.execution.Context

// TODO: Do we even WANT label types to be in a hierarchical relationship to each other?
class LabelType(val supertype: Option[LabelType]) extends DeclaredType with DeclaredType.DefinitionProperty[LabelDefinition] {
  // TODO: Once class types can extend label types, we need to unify the "direct declared subtypes" relation into
  //       one big tree that can hold both label types and class types.
  override def directDeclaredSubtypes(implicit context: Context): Set[Type] = {
    context.types.values.flatMap {
      case t: LabelType if t.supertype.contains(this) => Some(t)
      case _ => None
    }.toSet
  }

  /**
    * A label type is abstract unless it is an augmentation. That case is handled in the implementation of
    * intersection type's isAbstract.
    */
  override def isAbstract = true

  override def verbose = s"$toString < $supertype"
}
