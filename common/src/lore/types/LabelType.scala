package lore.types

trait LabelType extends DeclaredType {
  override def schema: LabelTypeSchema
  override def supertype: Option[LabelType]
  override def rootSupertype: LabelType = {
    // The compiler might not see this, but of course the root supertype of a label can itself only be a label type.
    super.rootSupertype.asInstanceOf[LabelType]
  }

  override def verbose = s"$toString${supertype.map(t => s" < $t").getOrElse("")}"
}
