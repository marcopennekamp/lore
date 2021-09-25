package lore.compiler.types

import lore.compiler.semantics.NamePath
import lore.compiler.semantics.structures.AliasDefinition
import lore.compiler.types.TypeVariable.Assignments

class AliasSchema(
  override val name: NamePath,
  override val parameters: Vector[TypeVariable],
  val originalType: Type,
) extends NamedSchema {
  var definition: AliasDefinition = _
  override def instantiate(assignments: Assignments): Type = Type.substitute(originalType, assignments)
}
