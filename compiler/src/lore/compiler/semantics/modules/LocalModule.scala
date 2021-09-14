package lore.compiler.semantics.modules

import lore.compiler.semantics.{NameKind, NamePath}

/**
  * A LocalModule is a lexical object that contains the import map and names of local declarations for a local module
  * declaration that surrounds its members in a fragment. It does not necessarily contain all declarations of the
  * semantic global module known via the name path.
  *
  * Precedence in the same module is skewed towards local declarations: local declarations > imports > parent local
  * declarations > parent imports > module members > parent module members. (Also consult the specification on this.)
  * This requires us to carry the local module forward throughout the compilation process, so that DeclNodes don't lose
  * their connection to the other local declarations. It also requires us to create a parent/child relationship between
  * nested local modules.
  */
case class LocalModule(
  modulePath: NamePath,
  parent: Option[LocalModule],
  localTypeNames: Set[String],
  localBindingNames: Set[String],
  importMap: LocalModule.ImportMap,
)(implicit globalModuleIndex: GlobalModuleIndex) {
  /**
    * Returns the full NamePath for the given simple name if it occurs in this local module, in one of the local
    * module's parents, or globally as a module member of the current module or one of its parents.
    *
    * To decide global membership, the [[GlobalModuleIndex]] is taken into consideration.
    */
  def getPath(memberName: String, nameKind: NameKind): Option[NamePath] = {
    lazy val memberPath = modulePath + memberName
    if (namesOf(nameKind).contains(memberName)) {
      Some(memberPath)
    } else {
      importMap
        .get(memberName)
        // TODO (modules): This `getPath` call will lead to the wrong order, because the parent's global module will be
        //                 consulted before this module's global module. We have to thus invoke a `getLocalPath` on the
        //                 parent that doesn't consult the global index.
        .orElse(parent.flatMap(_.getPath(memberName, nameKind)))
        .orElse(globalModuleIndex.getPath(memberPath, nameKind))
    }
  }

  private def namesOf(nameKind: NameKind): Set[String] = nameKind match {
    case NameKind.Type => localTypeNames
    case NameKind.Binding => localBindingNames
  }
}

object LocalModule {
  /**
    * An import map can immediately resolve the name paths for all imported simple names.
    */
  type ImportMap = Map[String, NamePath]
}
