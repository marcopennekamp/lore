package lore.compiler.semantics.modules

import lore.compiler.core.CompilationException
import lore.compiler.semantics.definitions.{BindingDefinition, BindingDefinitionKind}

/**
  * A multi-reference represents a reference to multiple definitions of the same simple name. The multi-reference may
  * only contain multiple definitions if its definition kind is multi-referable.
  *
  * Multi-references with multiple definitions must be disambiguated at some point. This disambiguation needs to be
  * performed, per the specification, by giving [[local]] definitions precedence over [[global]] definitions.
  */
case class MultiReference[A <: BindingDefinition](
  definitionKind: BindingDefinitionKind,
  local: Set[A],
  global: Set[A],
) {
  val bindings: Set[A] = local ++ global
  verify()

  /**
    * Returns the multi-reference's sole binding. Throws a [[CompilationException]] if the multi-reference has multiple
    * bindings.
    */
  def singleBinding: A = {
    if (bindings.size == 1) bindings.head
    else throw CompilationException("`MultiReference.singleBinding` cannot be used with multi-references containing" +
      " more than one binding.")
  }

  /**
    * Returns the multi-reference's sole binding, or `None` if the multi-reference has multiple bindings.
    */
  def singleBindingOption: Option[A] = if (bindings.size == 1) Some(bindings.head) else None

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
    if (bindings.isEmpty) {
      throw CompilationException("Multi-references must contain at least one member.")
    }

    if (definitionKind.isSingleReferable && bindings.size > 1) {
      throw CompilationException(s"Multi-references for single-referable bindings must be instantiated with a single" +
        s" module member. Definition kind: $definitionKind.")
    }

    if (bindings.map(_.simpleName).size > 1) {
      throw CompilationException(s"All members of a multi-reference must have the same simple name. Name paths:" +
        s" ${bindings.map(_.name).mkString(", ")}.")
    }
  }
}
