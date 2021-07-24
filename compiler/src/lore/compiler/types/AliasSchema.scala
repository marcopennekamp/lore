package lore.compiler.types

import lore.compiler.semantics.scopes.LocalTypeScope
import lore.compiler.types.TypeVariable.Assignments

class AliasSchema(
  override val name: String,
  override val typeScope: LocalTypeScope,
  val originalType: Type,
) extends NamedSchema with TypeSchema.TypeScoped {
  override protected def instantiate(assignments: Assignments): Type = Type.substitute(originalType, assignments)
}
