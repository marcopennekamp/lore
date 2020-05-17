package lore.runtime.types

trait DeclaredType extends lore.types.DeclaredType {
  // We need to override these because we have to assert to Scala that in the context of the runtime,
  // we are always expecting lore.runtime types.
  override def supertype: Option[DeclaredType]
  override def rootSupertype: DeclaredType = super.rootSupertype.asInstanceOf[DeclaredType]
}
