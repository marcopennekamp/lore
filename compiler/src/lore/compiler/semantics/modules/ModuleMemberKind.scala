package lore.compiler.semantics.modules

sealed trait ModuleMemberKind

object ModuleMemberKind {
  case object Type extends ModuleMemberKind
  case object Term extends ModuleMemberKind
}
