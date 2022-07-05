package lore.compiler.semantics.modules

import lore.compiler.semantics.definitions.{BindingDefinition, TermDefinition, TypeDefinition}

trait ModuleMembers[+A <: BindingDefinition]

object ModuleMembers {
  /**
    * Given `types` and `terms`, [[membersOfKind]] returns the [[ModuleMembers]] instance that contains members of kind
    * `moduleMemberKind`.
    */
  def membersOfKind[A <: BindingDefinition, M[_ <: BindingDefinition] <: ModuleMembers[_]](
    types: M[TypeDefinition],
    terms: M[TermDefinition],
    moduleMemberKind: ModuleMemberKind[A],
  ): M[A] = {
    moduleMemberKind match {
      case ModuleMemberKind.Type => types.asInstanceOf[M[A]]
      case ModuleMemberKind.Term => terms.asInstanceOf[M[A]]
    }
  }
}
