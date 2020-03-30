package lore.types

import lore.execution.Context

// TODO: Do we even WANT label types to be in a hierarchical relationship to each other?
case class LabelType(name: String, supertype: Type) extends DeclaredType {
  // TODO: Can class types extend label types? If so, we need to unify the "direct declared subtypes" relation into
  //       one big tree that can hold both label types and class types.
  override def directDeclaredSubtypes(implicit context: Context): Set[Type] = {
    context.types.values.filter(_.isInstanceOf[LabelType]).map(_.asInstanceOf[LabelType]).filter(_.supertype == this).toSet
  }

  /**
    * A label type is abstract unless it is an augmentation. That case is handled in the implementation of
    * intersection type abstractness.
    */
  override def isAbstract = true

  override def toString = s"$name"
  override def verbose = s"$toString < $supertype"
}
