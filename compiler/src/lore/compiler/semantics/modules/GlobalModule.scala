package lore.compiler.semantics.modules

import lore.compiler.semantics.NamePath
import lore.compiler.semantics.bindings.TermBinding

/**
  * A global module combines a module's definitions from across all source fragments. It also represents modules as a
  * palpable binding in scopes, so that module accesses can be resolved properly.
  */
class GlobalModule(val name: NamePath) extends TermBinding {
  val types: GlobalModuleMembers = new GlobalModuleMembers(this, ModuleMemberKind.Type)
  val terms: GlobalModuleMembers = new GlobalModuleMembers(this, ModuleMemberKind.Term)

  def members(memberKind: ModuleMemberKind): GlobalModuleMembers = memberKind match {
    case ModuleMemberKind.Type => types
    case ModuleMemberKind.Term => terms
  }

  override def toString: String = name.toString
}
