package lore.compiler.types

trait DeclaredType extends lore.types.DeclaredType {
  override def schema: DeclaredTypeSchema
  override def supertype: Option[DeclaredType]
  override def rootSupertype: DeclaredType = super.rootSupertype.asInstanceOf[DeclaredType]
}
