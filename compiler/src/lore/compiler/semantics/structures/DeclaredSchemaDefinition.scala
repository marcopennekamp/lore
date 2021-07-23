package lore.compiler.semantics.structures

import lore.compiler.types.DeclaredSchema

/**
  * A declared schema definition includes all definition components in their <i>uninstantiated</i> form. For example, a
  * struct definition contains all of the struct's properties with uninstantiated mentions of its type parameters.
  */
trait DeclaredSchemaDefinition extends SchemaDefinition {
  override def schema: DeclaredSchema
}
