package lore.compiler.semantics.modules

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.semantics.{BindingKind, NamePath}

/**
  * A LocalModule contains local declarations and supports name resolution by resolving simple names to local, global,
  * or imported module members. It does not necessarily contain all declarations of its global module, as a global
  * module may be split into many local modules.
  *
  * Name resolution relies on local modules to resolve simple names in a specific kind of order. (Consult the
  * specification for the details.) This requires us to carry the local module forward throughout the compilation
  * process, so that DeclNodes don't lose their connection to the other local declarations. It also requires us to
  * create a parent/child relationship between nested local modules.
  *
  * Local module members are separated by binding kind because some types and their corresponding modules may be placed
  * in different namespaces. This is the case with the String type, which as a basic type is placed in the root
  * namespace, but has the corresponding module `lore.String`.
  */
class LocalModule(
  val globalModule: GlobalModule,
  val parent: Option[LocalModule],
  typeMembers: Map[String, ModuleMember],
  termMembers: Map[String, ModuleMember],
  val position: Position,
)(implicit globalModuleIndex: GlobalModuleIndex) {
  // Per the specification, a local module must either have a parent or be the root module. This prevents the need to
  // treat the root module as a special case during name resolution.
  if (parent.isEmpty && globalModule != globalModuleIndex.root) {
    throw CompilationException(s"A local module must either be the root module or have a local module parent. Position:" +
      s" $position.")
  }

  val types: LocalModuleMembers = new LocalModuleMembers(this, typeMembers, ModuleMemberKind.Type)
  val terms: LocalModuleMembers = new LocalModuleMembers(this, termMembers, ModuleMemberKind.Term)

  /**
    * Turns a relative type path into an absolute type path. This works similar to type path resolution in TypeScopes.
    * If the name cannot be found, the function returns None.
    */
  def toAbsoluteTypePath(relativePath: NamePath): Option[NamePath] = {
    if (!relativePath.isMultiple) {
      types.getAbsolutePaths(relativePath.simpleName).map(_.singlePath)
    } else {
      // TODO (multi-import): An error should be reported if the head name does not refer to a module (or a companion
      //                      module through a struct binding). In general, reconsider the need for this function.
      terms
        .getAbsolutePaths(relativePath.headName)
        //.filter(_.bindingKind == BindingKind.Module)
        .map(_.singlePath ++ relativePath.tail)
        .filter(globalModuleIndex.has)
    }
  }

  def members(memberKind: ModuleMemberKind): LocalModuleMembers = memberKind match {
    case ModuleMemberKind.Type => types
    case ModuleMemberKind.Term => terms
  }
}
