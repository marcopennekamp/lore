package lore.compiler.semantics.modules

import lore.compiler.core.CompilationException
import lore.compiler.semantics.BindingKind

// TODO (multi-import): Add documentation comment.

case class MultiReference[A <: BindingModuleMember](
  bindingKind: BindingKind,
  local: Set[A],
  global: Set[A],
) {
  val members: Set[A] = local ++ global
  verify()

  /**
    * If the binding is single-referable, returns the multi-reference's only member. That there will only be a single
    * member is enforced by [[verify]]. If the binding is multi-referable, a [[CompilationException]] will be thrown.
    */
  def singleMember: A = {
    if (bindingKind.isSingleReferable) members.head
    else throw CompilationException("`MultiReference.singleMember` can only be used with single-referable bindings." +
      s" Binding kind: $bindingKind.")
  }

  /**
    * Whether this multi-reference is compatible with `other`, meaning that their binding kinds agree so that they
    * could be merged.
    */
  def isCompatibleWith(other: MultiReference[A]): Boolean = bindingKind == other.bindingKind

  /**
    * Concatenates this multi-reference with `other`. The binding kinds must agree.
    */
  def ++(other: MultiReference[A]): MultiReference[A] = {
    if (!this.isCompatibleWith(other)) {
      throw CompilationException(s"Binding kinds must agree for multi-reference concatenation. Own binding kind:" +
        s" $bindingKind. Other binding kind: ${other.bindingKind}.")
    }
    MultiReference(bindingKind, local ++ other.local, global ++ other.global)
  }

  /**
    * Creates a new multi-reference with `moduleMember` added as a local member.
    */
  def addLocal(moduleMember: A): MultiReference[A] = {
    if (moduleMember.bindingKind != bindingKind) {
      throw CompilationException(s"Cannot add module member ${moduleMember.name} to multi-reference: binding kinds" +
        s" don't agree. Own binding kind: $bindingKind. Member binding kind: ${moduleMember.bindingKind}.")
    }
    MultiReference(bindingKind, local + moduleMember, global)
  }

  private def verify(): Unit = {
    if (members.isEmpty) {
      throw CompilationException("Multi-references must contain at least one member.")
    }

    if (bindingKind.isSingleReferable && members.size > 1) {
      throw CompilationException(s"Multi-references for single-referable bindings must be instantiated with a single" +
        s" module member. Binding kind: $bindingKind.")
    }

    if (members.map(_.simpleName).size > 1) {
      throw CompilationException(s"All members of a multi-reference must have the same simple name. Name paths:" +
        s" ${members.map(_.name).mkString(", ")}.")
    }
  }
}
