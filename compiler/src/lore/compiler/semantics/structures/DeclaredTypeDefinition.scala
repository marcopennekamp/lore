package lore.compiler.semantics.structures

import lore.compiler.types.DeclaredType

trait DeclaredTypeDefinition extends TypeDefinition {
  override def tpe: DeclaredType
}
