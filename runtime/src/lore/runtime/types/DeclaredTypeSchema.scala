package lore.runtime.types

trait DeclaredTypeSchema extends lore.types.DeclaredTypeSchema {
  // We need to override these because we have to assert to Scala that in the context of the runtime,
  // we are always expecting lore.runtime types.
  override def superschema: Option[DeclaredTypeSchema]
  override def rootSuperschema: DeclaredTypeSchema = super.rootSuperschema.asInstanceOf[DeclaredTypeSchema]
}
