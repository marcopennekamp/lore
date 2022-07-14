package lore.compiler.semantics.definitions

import lore.compiler.semantics.BindingKind
import lore.compiler.semantics.bindings.{StructBinding, TypeBinding}
import lore.compiler.syntax.TypeDeclNode
import lore.compiler.utils.CollectionExtensions.OptionExtension

/**
  * A user-defined type definition (or basic type) that can be used as a type binding.
  */
trait TypeDefinition extends BindingDefinition with TypeBinding with HasLocalModule {
  override def node: TypeDeclNode

  /**
    * The struct binding belonging to this type definition. This is only defined for struct schemas and struct aliases.
    *
    * TODO (multi-import): This should probably be a Once and initialized during global module resolution. There's no
    *                      need to go through the global module, really.
    */
  def structBinding: Option[StructBinding] = globalModule.terms.get(simpleName).filterType[StructBinding]

  override def bindingKind: BindingKind = BindingKind.Type
}
