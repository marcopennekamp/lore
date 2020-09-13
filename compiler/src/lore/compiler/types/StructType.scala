package lore.compiler.types

import lore.compiler.semantics.structures.ClassDefinition

/**
  * The struct's component types are part of the supertypes list of the struct.
  */
class StructType(
  override val name: String,
  override val supertypes: Vector[Type],
) extends DeclaredType with DeclaredType.DefinitionProperty[ClassDefinition] {
  /**
    * Only entities of the ownedBy type may own a component of this class type.
    */
  def ownedBy: Option[Type] = definition.ownedBy
}
