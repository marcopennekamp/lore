package lore.types

// TODO: Why are label types declared as a schema if they can't take type arguments anyway? The reason is
//       future-proofing:

/**
  * A type schema for label types.
  *
  * Why are label types declared as a schema if they can't take type arguments anyway? The reason is
  * future-proofing: I think being able to give type arguments to label types will be very useful in
  * the long term. And if not, we can always refactor the compiler and runtime to move this detail
  * to classes.
  */
trait LabelTypeSchema extends DeclaredTypeSchema with TypeSchema[LabelType] {
  /**
    * A label type is abstract unless it is an augmentation. That case is handled in the implementation of
    * intersection type's isAbstract.
    */
  override def isAbstract = true

  override def superschema: Option[LabelTypeSchema]
  override def rootSuperschema: LabelTypeSchema = super.rootSuperschema.asInstanceOf[LabelTypeSchema]
}
