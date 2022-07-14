package lore.compiler.semantics.modules

import lore.compiler.core.CompilationException
import lore.compiler.semantics.definitions.{BindingDefinition, BindingDefinitionKind}

// TODO (multi-import): Add documentation comment.

case class MultiReference[A <: BindingDefinition](
  definitionKind: BindingDefinitionKind,
  local: Set[A],
  global: Set[A],
) {
  val members: Set[A] = local ++ global
  verify()

  /**
    * If the multi-reference is single-referable, returns the multi-reference's only binding. That there will only be a
    * single binding is enforced by [[verify]]. If the multi-reference is multi-referable, a [[CompilationException]]
    * will be thrown.
    */
  def singleBinding: A = {
    if (definitionKind.isSingleReferable) members.head
    else throw CompilationException("`MultiReference.singleMember` can only be used with single-referable bindings." +
      s" Definition kind: $definitionKind.")
  }

  /**
    * Whether this multi-reference is compatible with `other`, meaning that their definition kinds agree so that they
    * could be merged.
    */
  def isCompatibleWith(other: MultiReference[A]): Boolean = definitionKind == other.definitionKind

  /**
    * Concatenates this multi-reference with `other`. The definition kinds must agree.
    */
  def ++(other: MultiReference[A]): MultiReference[A] = {
    if (!this.isCompatibleWith(other)) {
      throw CompilationException(s"Definition kinds must agree for multi-reference concatenation. Own definition kind:" +
        s" $definitionKind. Other definition kind: ${other.definitionKind}.")
    }
    MultiReference(definitionKind, local ++ other.local, global ++ other.global)
  }

  /**
    * Creates a new multi-reference with `moduleMember` added as a local member.
    */
  def addLocal(moduleMember: A): MultiReference[A] = {
    if (moduleMember.definitionKind != definitionKind) {
      throw CompilationException(s"Cannot add module member ${moduleMember.name} to multi-reference: definition kinds" +
        s" don't agree. Own definition kind: $definitionKind. Member definition kind: ${moduleMember.definitionKind}.")
    }
    MultiReference(definitionKind, local + moduleMember, global)
  }

  private def verify(): Unit = {
    if (members.isEmpty) {
      throw CompilationException("Multi-references must contain at least one member.")
    }

    if (definitionKind.isSingleReferable && members.size > 1) {
      throw CompilationException(s"Multi-references for single-referable bindings must be instantiated with a single" +
        s" module member. Definition kind: $definitionKind.")
    }

    if (members.map(_.simpleName).size > 1) {
      throw CompilationException(s"All members of a multi-reference must have the same simple name. Name paths:" +
        s" ${members.map(_.name).mkString(", ")}.")
    }
  }
}
