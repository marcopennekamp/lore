package lore.compiler.types

import lore.compiler.semantics.structures.StructDefinition

class StructSchema(
  override val name: String,
  override val parameters: Vector[TypeVariable],
  override val supertypes: Vector[Type],
) extends DeclaredSchema with DeclaredSchema.DefinitionProperty[StructDefinition] {
  override def representative: StructType = super.representative.asInstanceOf[StructType]
  override def instantiate(assignments: TypeVariable.Assignments): StructType = StructType(this, assignments)

  def hasOpenProperties: Boolean = definition.openProperties.nonEmpty
}
