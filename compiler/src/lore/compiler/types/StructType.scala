package lore.compiler.types

import lore.compiler.semantics.structures.StructDefinition

/**
  * @param supertypes The list of supertypes of the struct, which include all traits the struct implements.
  */
class StructType(
  override val name: String,
  override val supertypes: Vector[Type],
) extends DeclaredType with DeclaredType.DefinitionProperty[StructDefinition]
