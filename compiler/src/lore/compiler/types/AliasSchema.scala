package lore.compiler.types

import lore.compiler.types.TypeVariable.Assignments

class AliasSchema(
  override val name: String,
  override val parameters: Vector[TypeVariable],
  val originalType: Type,
) extends NamedSchema {
  override def instantiate(assignments: Assignments): Type = Type.substitute(originalType, assignments)
}
