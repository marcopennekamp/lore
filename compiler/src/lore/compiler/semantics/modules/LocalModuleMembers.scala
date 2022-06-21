package lore.compiler.semantics.modules

import lore.compiler.core.CompilationException
import lore.compiler.semantics.{BindingKind, MultiNamePath}

/**
  * [[LocalModuleMembers]] manages local module members of a particular binding kind, i.e. either types or terms.
  */
class LocalModuleMembers(
  val localModule: LocalModule,
  val members: Map[String, ModuleMember],
  val memberKind: ModuleMemberKind,
)(implicit globalModuleIndex: GlobalModuleIndex) {
  private val parent: Option[LocalModuleMembers] = localModule.parent.map(_.members(memberKind))
  private val global: GlobalModuleMembers = localModule.globalModule.members(memberKind)

  /**
    * For each simple name, all local and foreign module members that are accessible by simple name in this local
    * module without taking parent modules into account. The map is built by considering local module members and
    * imports, while handling multi-referable members, the precedence of local members, and precedence between imports.
    *
    * [[accessibles]] is mutable so that it can be constructed efficiently. It should only be changed during module
    * resolution.
    */
  var accessibles: Map[String, MultiNamePath] = members.map {
    case (name, moduleMember) => name -> MultiNamePath(Set(moduleMember.namePath), Set.empty, moduleMember.bindingKind)
  }

  /**
    * Returns a [[MultiNamePath]] for `memberName` if it occurs in this local module, in one of the local module's
    * parents, or globally as a module member of the current local module or a parent local module. Multi-referable
    * bindings may result in a multi name path that contains multiple name paths, while single-referable bindings are
    * guaranteed to produce a single name path. The rules governing name resolution of name paths, especially multi
    * name paths, are defined in the language specification.
    *
    * Per the specification, globally declared members of contracted module names (e.g. `foo` in `module foo.bar`) are
    * not taken into account. To decide global membership, the [[GlobalModuleIndex]] is taken into consideration.
    */
  def getAbsolutePaths(memberName: String): Option[MultiNamePath] = {
    getAbsolutePathsLocally(memberName) match {
      case Some(localPaths) if localPaths.bindingKind.isMultiReferable =>
        val paths = getAbsolutePathsGlobally(memberName, Some(localPaths.bindingKind)) match {
          case Some(globalPaths) => globalPaths ++ localPaths
          case None => localPaths
        }
        Some(paths)
      case Some(localPaths) => Some(localPaths)
      case None => getAbsolutePathsGlobally(memberName, None)
    }
  }

  /**
    * Returns a [[MultiNamePath]] for `memberName` if it occurs in this local module or a parent local module.
    * Multi name paths from multi-referable bindings are merged across levels.
    */
  private def getAbsolutePathsLocally(memberName: String): Option[MultiNamePath] = {
    accessibles.get(memberName) match {
      case Some(paths) => Some(mergeWithParentPaths(paths, _.getAbsolutePathsLocally(memberName)))
      case None => parent.flatMap(_.getAbsolutePathsLocally(memberName))
    }
  }

  /**
    * Returns a [[MultiNamePath]] for `memberName` if it occurs in this local module's global definitions, or those of
    * a parent local module or the root module.
    *
    * If `localBindingKind` is specified, the member has already been found locally, but the bindings are
    * multi-referable and thus the global space has to be searched as well. In this case, care has to be taken that the
    * specification's requirement of local shadowing is followed. The parent chain for a global module may only be
    * followed so far as the `localBindingKind` still agrees with the local member's *and* global member's binding
    * kinds. See the section `Interaction with other bindings` in `modules.md` of the specification for a motivating
    * example.
    */
  protected def getAbsolutePathsGlobally(memberName: String, localBindingKind: Option[BindingKind]): Option[MultiNamePath] = {
    if (localBindingKind.exists(_.isSingleReferable)) {
      throw CompilationException("`localBindingKind` must be multi-referable. Otherwise, `getAbsolutePathsGlobally`" +
        " shouldn't have been called as the local module member should've been preferred.")
    }

    // Check that `localBindingKind`, if it exists, agrees with the binding kind of the local module's member, if it
    // exists. Keep in mind that `localBindingKind` may not have come from THIS local module, so we have to check
    // against it in all cases.
    if (localBindingKind.exists(bk => accessibles.get(memberName).exists(_.bindingKind != bk))) {
      return None
    }

    global.get(memberName) match {
      case Some(moduleMember) if localBindingKind.exists(_ != moduleMember.bindingKind) => None
      case Some(moduleMember) =>
        val multiNamePath = MultiNamePath(Set.empty, Set(moduleMember.namePath), moduleMember.bindingKind)
        Some(mergeWithParentPaths(multiNamePath, _.getAbsolutePathsGlobally(memberName, localBindingKind)))
      case None =>
        parent.flatMap(_.getAbsolutePathsGlobally(memberName, localBindingKind))
    }
  }

  /**
    * Merges `multiNamePath` with any paths received from `getParentPaths` *if* `multiNamePath` is multi-referable and
    * the parent paths agree in their binding kind with `multiNamePath`. Otherwise, `multiNamePath` is preferred.
    */
  private def mergeWithParentPaths(
    multiNamePath: MultiNamePath,
    getParentPaths: LocalModuleMembers => Option[MultiNamePath],
  ): MultiNamePath = {
    if (multiNamePath.bindingKind.isMultiReferable && parent.isDefined) {
      getParentPaths(parent.get)
        .filter(multiNamePath.isCompatibleWith)
        .map(multiNamePath ++ _)
        .getOrElse(multiNamePath)
    } else multiNamePath
  }
}
