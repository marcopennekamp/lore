package lore.compiler.semantics.definitions

import lore.compiler.semantics.bindings.{StructBinding, TypeBinding}
import lore.compiler.syntax.TypeDeclNode

/**
  * A user-defined type definition (or basic type) that can be used as a type binding.
  */
trait TypeDefinition extends BindingDefinition with TypeBinding with HasLocalModule {
  override def node: TypeDeclNode

  /**
    * The struct binding belonging to this type definition. This is only defined for struct schemas and struct aliases.
    */
  var structBinding: Option[StructBinding] = None

  override def definitionKind: BindingDefinitionKind = BindingDefinitionKind.Type
}
