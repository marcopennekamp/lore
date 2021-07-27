package lore.compiler.types

import lore.compiler.semantics.structures.TraitDefinition

class TraitSchema(
  override val name: String,
  override val parameters: Vector[TypeVariable],
  override val supertypes: Vector[Type],
) extends DeclaredSchema with DeclaredSchema.DefinitionProperty[TraitDefinition] {
  override def representative: TraitType = super.representative.asInstanceOf[TraitType]
  override def instantiate(assignments: TypeVariable.Assignments): TraitType = TraitType(this, assignments)
}
