package lore.compiler.semantics.modules

import lore.compiler.core.CompilationException
import lore.compiler.semantics.definitions.{BindingDefinition, BindingDefinitionKind}

/**
  * A multi-reference represents a reference to multiple definitions of the same simple name. The multi-reference may
  * only contain multiple definitions if its definition kind is multi-referable. The [[local]] and [[global]] lists may
  * not contain duplicates and [[global]] should not contain bindings that are already contained in [[local]].
  *
  * Multi-references with multiple definitions must be disambiguated at some point. This disambiguation needs to be
  * performed, per the specification, by giving [[local]] definitions precedence over [[global]] definitions.
  */
case class MultiReference[+A <: BindingDefinition](
  definitionKind: BindingDefinitionKind,
  local: Vector[A],
  global: Vector[A],
) {
  val bindings: Vector[A] = local ++ global
  verify()

  def simpleName: String = bindings.head.simpleName
  def isSingle: Boolean = bindings.size == 1

  /**
    * Returns the multi-reference's sole binding. Throws a [[CompilationException]] if the multi-reference has multiple
    * bindings.
    */
  def singleBinding: A = {
    if (isSingle) bindings.head
    else throw CompilationException("`MultiReference.singleBinding` cannot be used with multi-references containing" +
      " more than one binding.")
  }

  /**
    * Returns the multi-reference's sole binding, or `None` if the multi-reference has multiple bindings.
    */
  def singleBindingOption: Option[A] = if (isSingle) Some(bindings.head) else None

  /**
    * Whether this multi-reference is compatible with `other`, meaning that their definition kinds agree so that they
    * could be merged.
    */
  def isCompatibleWith[B <: BindingDefinition](other: MultiReference[B]): Boolean = definitionKind == other.definitionKind

  /**
    * Concatenates this multi-reference with `other`. The definition kinds must agree.
    */
  def ++[B >: A <: BindingDefinition](other: MultiReference[B]): MultiReference[B] = {
    if (!this.isCompatibleWith(other)) {
      throw CompilationException(s"Definition kinds must agree for multi-reference concatenation. Own definition kind:" +
        s" $definitionKind. Other definition kind: ${other.definitionKind}.")
    }

    // The `filterNot` removes local members from global members, which might have been present before. This ensures
    // that global members don't contain local members.
    val local2 = (local ++ other.local).distinct
    val global2 = (global ++ other.global).distinct.filterNot(local2.contains)
    MultiReference(definitionKind, local2, global2)
  }

  /**
    * Creates a new multi-reference with `moduleMember` added as a local member.
    */
  def addLocal[B >: A <: BindingDefinition](moduleMember: B): MultiReference[B] = {
    if (moduleMember.definitionKind != definitionKind) {
      throw CompilationException(s"Cannot add module member ${moduleMember.name} to multi-reference: definition kinds" +
        s" don't agree. Own definition kind: $definitionKind. Member definition kind: ${moduleMember.definitionKind}.")
    }

    if (!local.contains(moduleMember)) {
      // The `filterNot` removes the new local member from the global members, if it was present before. This ensures
      // that global members don't contain local members.
      MultiReference(definitionKind, local :+ moduleMember, global.filterNot(_ == moduleMember))
    } else this
  }

  private def verify(): Unit = {
    // TODO (multi-import): These checks might be quite expensive, especially when multi-references are constructed
    //                      piece-by-piece. Consider disabling them in production builds, but profile first.
    if (bindings.isEmpty) {
      throw CompilationException("Multi-references must contain at least one member.")
    }

    if (definitionKind.isSingleReferable && bindings.size > 1) {
      throw CompilationException(s"Multi-references for single-referable bindings must be instantiated with a single" +
        s" module member. Definition kind: $definitionKind.")
    }

    if (bindings.map(_.simpleName).distinct.size > 1) {
      throw CompilationException(s"All members of a multi-reference must have the same simple name. Name paths:" +
        s" ${bindings.map(_.name).mkString(", ")}.")
    }

    if (local.exists(global.contains)) {
      throw CompilationException(s"A multi-reference's global members should not contain a local member. Local:" +
        s" ${local.map(_.name).mkString(", ")}. Global: ${global.map(_.name).mkString(", ")}.")
    }
  }
}
