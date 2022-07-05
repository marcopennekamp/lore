package lore.compiler.semantics.modules

import lore.compiler.semantics.definitions.{BindingDefinition, TermDefinition, TypeDefinition}

sealed trait ModuleMemberKind[A <: BindingDefinition]

object ModuleMemberKind {
  case object Type extends ModuleMemberKind[TypeDefinition]
  case object Term extends ModuleMemberKind[TermDefinition]
}
