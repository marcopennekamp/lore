package lore.compiler.types

import lore.compiler.semantics.structures.StructDefinition

class StructSchema(
  override val name: String,
  override val parameters: Vector[TypeVariable],
  override val supertypes: Vector[Type],
) extends DeclaredSchema with DeclaredSchema.DefinitionProperty[StructDefinition] {

  override protected def instantiate(assignments: TypeVariable.Assignments): StructType = StructType(this, assignments)

}
