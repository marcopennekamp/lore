package lore.compiler.semantics.modules

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.semantics.BindingKind

/**
  * [[GlobalModuleMembers]] manages global module members of a particular binding kind, i.e. either types or terms.
  */
class GlobalModuleMembers(val globalModule: GlobalModule, val memberKind: ModuleMemberKind) {
  var members: Map[String, ModuleMember] = Map.empty

  /**
    * Adds a binding `memberName` from `node` to the global module. `node` may be `None`, which will create a module
    * member without any associated nodes.
    *
    * If another module member with the same name already exists, a compilation exception is thrown. This is
    * circumvented by multi-definable bindings. Another edge case is concerned with structs and companion modules:
    * Adding a struct type `memberName` overrides an already existing module `memberName`.
    */
  def add(memberName: String, bindingKind: BindingKind, position: Option[Position]): ModuleMember = {
    val member = members.get(memberName) match {
      case Some(member) =>
        if (bindingKind == BindingKind.Struct && member.bindingKind == BindingKind.Module) {
          // Edge case: We're adding a struct to an existing module, which means that the module is a companion module.
          val structMember = new ModuleMember(member.namePath, bindingKind)
          members += memberName -> structMember
          structMember
        } else if (bindingKind == BindingKind.Module && member.bindingKind == BindingKind.Struct) {
          // Edge case: A struct has already been added and we don't need to add the companion module.
          member
        } else if (!member.bindingKind.isMultiDefinable || member.bindingKind != bindingKind) {
          throw CompilationException(s"Cannot add $bindingKind $memberName to global module ${globalModule.name}:" +
            s" $memberName is already declared as a ${member.bindingKind}.")
        } else {
          member
        }

      case None =>
        val member = new ModuleMember(globalModule.name + memberName, bindingKind)
        members += memberName -> member
        member
    }

    position.foreach(member.addPosition)
    member
  }

  /**
    * Returns the module member called `memberName`, or `None` otherwise.
    */
  def get(memberName: String): Option[ModuleMember] = members.get(memberName)

  /**
    * Whether this module has a member `memberName`.
    */
  def has(memberName: String): Boolean = members.contains(memberName)
}
