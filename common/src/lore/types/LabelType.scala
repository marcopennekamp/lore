package lore.types

trait LabelType extends DeclaredType {
  /**
    * A label type is abstract unless it is an augmentation. That case is handled in the implementation of
    * intersection type's isAbstract.
    */
  override def isAbstract = true

  override def verbose = s"$toString${supertype.map(t => s" < $t").getOrElse("")}"
}
