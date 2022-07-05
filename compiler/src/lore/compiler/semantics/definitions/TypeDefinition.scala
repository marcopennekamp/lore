package lore.compiler.semantics.definitions

import lore.compiler.semantics.BindingKind
import lore.compiler.semantics.bindings.TypeBinding
import lore.compiler.syntax.TypeDeclNode

/**
  * A user-defined type definition (or basic type) that can be used as a type binding.
  */
trait TypeDefinition extends BindingDefinition with TypeBinding with HasLocalModule {
  override def node: TypeDeclNode

  override def bindingKind: BindingKind = BindingKind.Type
}
