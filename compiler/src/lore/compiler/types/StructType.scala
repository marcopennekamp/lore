package lore.compiler.types

import lore.compiler.semantics.structures.StructDefinition

/**
  * @param supertypes The list of supertypes of the struct, which do not only include all traits the struct
  *                   implements but also all component types that are backed by a component declared in the
  *                   struct. For example, if we have a struct implementing the trait A and it has components
  *                   C1 and C2, its supertypes list is [A, +C1, +C2].
  */
class StructType(
  override val name: String,
  override val supertypes: Vector[Type],
) extends DeclaredType with DeclaredType.DefinitionProperty[StructDefinition] {
  /**
    * Only entities of the ownedBy type may own a component of this struct type.
    */
  // TODO: Reimplement ownedBy.
  //def ownedBy: Option[Type] = definition.ownedBy
}
