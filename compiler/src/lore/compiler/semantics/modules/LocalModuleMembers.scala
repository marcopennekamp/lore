package lore.compiler.semantics.modules

import lore.compiler.core.CompilationException
import lore.compiler.semantics.definitions.BindingDefinition
import lore.compiler.semantics.{BindingKind, Registry}

/**
  * [[LocalModuleMembers]] manages type or term local module members.
  */
class LocalModuleMembers[A <: BindingDefinition](
  val localModule: LocalModule,
  val members: Map[String, A],
  val moduleMemberKind: ModuleMemberKind[A],
)(implicit registry: Registry) extends ModuleMembers[A] {
  private val parent: Option[LocalModuleMembers[A]] = localModule.parent.map(_.members(moduleMemberKind))
  private val global: GlobalModuleMembers[A] = localModule.globalModule.members(moduleMemberKind)

  /**
    * For each simple name, all local and foreign module members that are accessible by simple name in this local
    * module without taking parent modules into account. The map is built by considering local module members and
    * imports, while handling multi-referable members, the precedence of local members, and precedence between imports.
    *
    * [[accessibles]] is mutable so that it can be constructed efficiently. It should only be mutated during module
    * resolution.
    */
  var accessibles: Map[String, MultiReference[A]] = members.map {
    case (name, moduleMember) => name -> MultiReference(moduleMember.bindingKind, Set(moduleMember), Set.empty)
  }

  /**
    * Returns a [[MultiReference]] for `memberName` if it occurs in this local module, in one of the local module's
    * parents, or globally as a module member of the current local module or a parent local module. Multi-referable
    * bindings may result in a multi-reference that contains multiple members, while single-referable bindings are
    * guaranteed to produce a single member. The rules governing name resolution are defined in the language
    * specification.
    *
    * Per the specification, globally declared members of contracted module names (e.g. `foo` in `module foo.bar`) are
    * not taken into account. To decide global membership, the [[Registry]] is taken into consideration.
    */
  def getAccessibleMembers(memberName: String): Option[MultiReference[A]] = {
    getAccessibleMembersLocally(memberName) match {
      case Some(localMembers) if localMembers.bindingKind.isMultiReferable =>
        val members = getAccessibleMembersGlobally(memberName, Some(localMembers.bindingKind)) match {
          case Some(globalMembers) => globalMembers ++ localMembers
          case None => localMembers
        }
        Some(members)
      case Some(localMembers) => Some(localMembers)
      case None => getAccessibleMembersGlobally(memberName, None)
    }
  }

  /**
    * Returns a [[MultiReference]] for `memberName` if it occurs in this local module or a parent local module.
    * Multi-references from multi-referable bindings are merged across levels.
    */
  private def getAccessibleMembersLocally(memberName: String): Option[MultiReference[A]] = {
    accessibles.get(memberName) match {
      case Some(multiReference) => Some(mergeWithParentMembers(multiReference, _.getAccessibleMembersLocally(memberName)))
      case None => parent.flatMap(_.getAccessibleMembersLocally(memberName))
    }
  }

  /**
    * Returns a [[MultiReference]] for `memberName` if it occurs in this local module's global definitions, or those of
    * a parent local module or the root module.
    *
    * If `localBindingKind` is specified, the member has already been found locally, but the bindings are
    * multi-referable and thus the global space has to be searched as well. In this case, care has to be taken that the
    * specification's requirement of local shadowing is followed. The parent chain for a global module may only be
    * followed so far as the `localBindingKind` still agrees with the local member's <i>and</i> global member's binding
    * kinds. See the section `Interaction with other bindings` in `modules.md` of the specification for a motivating
    * example.
    */
  protected def getAccessibleMembersGlobally(
    memberName: String,
    localBindingKind: Option[BindingKind],
  ): Option[MultiReference[A]] = {
    if (localBindingKind.exists(_.isSingleReferable)) {
      throw CompilationException("`localBindingKind` must be multi-referable. Otherwise, `getAccessibleMembersGlobally`" +
        " shouldn't have been called as the local module member should've been preferred.")
    }

    // Check that `localBindingKind`, if it exists, agrees with the binding kind of the local module's member, if it
    // exists. Keep in mind that `localBindingKind` may not have come from THIS local module but rather a parent local
    // module, so we have to check against it in all cases.
    if (localBindingKind.exists(bk => accessibles.get(memberName).exists(_.bindingKind != bk))) {
      return None
    }

    global.get(memberName) match {
      case Some(moduleMember) if localBindingKind.exists(_ != moduleMember.bindingKind) => None
      case Some(moduleMember) =>
        val multiReference = MultiReference(moduleMember.bindingKind, Set.empty, Set(moduleMember))
        Some(mergeWithParentMembers(multiReference, _.getAccessibleMembersGlobally(memberName, localBindingKind)))
      case None =>
        parent.flatMap(_.getAccessibleMembersGlobally(memberName, localBindingKind))
    }
  }

  /**
    * Merges `multiReference` with any multi-references received from `mergeWithParentMembers` *if* `multiReference` is
    * multi-referable and the parent multi-reference agrees in its binding kind with `multiReference`. Otherwise,
    * `multiReference` has precedence.
    */
  private def mergeWithParentMembers(
    multiReference: MultiReference[A],
    getParentMembers: LocalModuleMembers[A] => Option[MultiReference[A]],
  ): MultiReference[A] = {
    if (multiReference.bindingKind.isMultiReferable && parent.isDefined) {
      getParentMembers(parent.get)
        .filter(multiReference.isCompatibleWith)
        .map(multiReference ++ _)
        .getOrElse(multiReference)
    } else multiReference
  }
}
