package lore.compiler.types

import lore.compiler.semantics.scopes.LocalTypeScope
import lore.compiler.semantics.structures.StructDefinition

class StructSchema(
  override val name: String,
  override val typeScope: LocalTypeScope,
  override val supertypes: Vector[Type],
) extends DeclaredSchema with DeclaredSchema.DefinitionProperty[StructDefinition] {

  override protected def instantiate(assignments: TypeVariable.Assignments): StructType = {
    StructType(this, assignments)
  }

}
