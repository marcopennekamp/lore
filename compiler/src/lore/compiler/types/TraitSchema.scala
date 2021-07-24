package lore.compiler.types

import lore.compiler.semantics.scopes.LocalTypeScope
import lore.compiler.semantics.structures.TraitDefinition

class TraitSchema(
  override val name: String,
  override val typeScope: LocalTypeScope,
  override val supertypes: Vector[Type],
) extends DeclaredSchema with DeclaredSchema.DefinitionProperty[TraitDefinition] {

  override protected def instantiate(assignments: TypeVariable.Assignments): TraitType = TraitType(this, assignments)

}
