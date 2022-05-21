package lore.compiler.semantics.modules

import lore.compiler.core.Position
import lore.compiler.semantics.modules.LocalModule.ImportMap
import lore.compiler.semantics.{NameKind, NamePath}
import lore.compiler.syntax.DeclNode

/**
  * A LocalModule is a lexical object that contains the import maps and names of local declarations for a local module
  * declaration that surrounds its members in a fragment. It does not necessarily contain all declarations of the
  * semantic global module known via the name path.
  *
  * Precedence in the same module is skewed towards local declarations: local declarations > imports > parent local
  * declarations > parent imports > module members > root module members. (Also consult the specification on this.)
  * This requires us to carry the local module forward throughout the compilation process, so that DeclNodes don't lose
  * their connection to the other local declarations. It also requires us to create a parent/child relationship between
  * nested local modules.
  *
  * Import maps are separated by name kind because some types and their corresponding modules may be placed in
  * different namespaces. This is the case with the String type, which as a basic type is placed in the root namespace,
  * and the corresponding module `lore.String`.
  *
  * @param localTypeNames Even though these names can be derived from `members`, we don't want `localTypeNames` and
  *                       `localBindingNames` to have to be recreated every time a LocalModule is copied (for
  *                       successively populating the import maps).
  */
case class LocalModule(
  modulePath: NamePath,
  parent: Option[LocalModule],
  members: Vector[DeclNode],
  localTypeNames: Set[String],
  localBindingNames: Set[String],
  typeImportMap: ImportMap,
  bindingImportMap: ImportMap,
  position: Position,
)(implicit globalModuleIndex: GlobalModuleIndex) {
  /**
    * Returns an absolute name path for `memberName` if it occurs in this local module, in one of the local module's
    * parents, or globally as a module member of the current local module or a parent local module. Per the
    * specification, globally declared members of contracted module names (e.g. `foo` in `module foo.bar`) are not
    * taken into account.
    *
    * To decide global membership, the [[GlobalModuleIndex]] is taken into consideration.
    */
  def getAbsolutePath(memberName: String, nameKind: NameKind): Option[NamePath] = {
    getAbsolutePathLocally(memberName, nameKind).orElse(getAbsolutePathGlobally(memberName, nameKind))
  }

  /**
    * Returns an absolute name path for `memberName` if it occurs in this local module or a parent local module, either
    * as a declaration or an import.
    */
  protected def getAbsolutePathLocally(memberName: String, nameKind: NameKind): Option[NamePath] = {
    if (namesOf(nameKind).contains(memberName)) {
      Some(modulePath + memberName)
    } else {
      importMapOf(nameKind).get(memberName).orElse(
        parent.flatMap(_.getAbsolutePathLocally(memberName, nameKind))
      )
    }
  }

  /**
    * Returns an absolute name path for `memberName` if it occurs in this local module's global definitions, or those
    * of a parent local module or the root module.
    */
  protected def getAbsolutePathGlobally(memberName: String, nameKind: NameKind): Option[NamePath] = {
    globalModuleIndex.getModule(modulePath).flatMap(_.getAbsolutePath(memberName, nameKind)).orElse {
      parent match {
        case Some(parent) => parent.getAbsolutePathGlobally(memberName, nameKind)
        case None => globalModuleIndex.root.getAbsolutePath(memberName, nameKind)
      }
    }
  }

  /**
    * Turns a relative type path into an absolute type path. This works similar to type path resolution in TypeScopes.
    * If the name cannot be found, the function returns None.
    */
  def toAbsoluteTypePath(relativePath: NamePath): Option[NamePath] = {
    if (!relativePath.isMultiple) {
      getAbsolutePath(relativePath.simpleName, NameKind.Type)
    } else {
      getAbsolutePath(relativePath.headName, NameKind.Binding)
        .map(_ ++ relativePath.tail)
    }
  }

  private def namesOf(nameKind: NameKind): Set[String] = nameKind match {
    case NameKind.Type => localTypeNames
    case NameKind.Binding => localBindingNames
  }

  private def importMapOf(nameKind: NameKind): ImportMap = nameKind match {
    case NameKind.Type => typeImportMap
    case NameKind.Binding => bindingImportMap
  }
}

object LocalModule {
  /**
    * An import map can immediately resolve the name paths for all imported simple names.
    */
  type ImportMap = Map[String, NamePath]
}
