package lore.compiler.types

import lore.compiler.semantics.NamePath
import lore.compiler.semantics.structures.TraitDefinition
import lore.compiler.types.NamedSchema.DefinitionProperty

class TraitSchema(
  override val name: NamePath,
  override val parameters: Vector[TypeVariable],
  override val supertypes: Vector[Type],
) extends DeclaredSchema with DefinitionProperty[TraitDefinition] {
  override val kind: Kind = Kind.Trait
  override def constantType: TraitType = super.constantType.asInstanceOf[TraitType]
  override def instantiate(assignments: TypeVariable.Assignments): TraitType = TraitType(this, assignments)
}
