package lore.compiler.semantics.modules

import lore.compiler.semantics.NamePath

/**
  * A LexicalModule is a lexical object that contains the import map and names of local declarations for an actual
  * module declaration that surrounds its members in a fragment. It does not necessarily contain all declarations of
  * the semantic module known via the name path.
  *
  * Precedence in the same module is skewed towards local declarations: local declarations > imports > parent local
  * declarations > parent imports > module members > parent module members. This requires us to carry the LexicalModule
  * forward throughout the compilation process, so that DeclNodes don't lose their connection to the other local
  * declarations. It also requires us to create a parent/child relationship between nested lexical modules.
  *
  * TODO (modules): Rename to LocalModule?
  */
case class LexicalModule(
  modulePath: NamePath,
  parent: Option[LexicalModule],
  importMap: Map[String, NamePath],
  localBindingNames: Set[String],
  localTypeNames: Set[String],
)(implicit globalModuleIndex: GlobalModuleIndex) {
  /**
    * This function is the type-centric version of [[getPath]].
    */
  def getTypePath(memberName: String): Option[NamePath] = {
    getPath(_.localTypeNames, _.typeNames)(memberName)
  }

  /**
    * This function is the binding-centric version of [[getPath]]
    */
  def getBindingPath(memberName: String): Option[NamePath] = {
    getPath(_.localBindingNames, _.bindingNames)(memberName)
  }

  /**
    * Returns the full NamePath for the given type name if it occurs in this lexical module, in one of the lexical
    * module's parents, or globally as a module member of the current module or one of its parents.
    *
    * To decide global membership, the [[GlobalModuleIndex]] is taken into consideration.
    */
  def getPath(
    getNames: LexicalModule => Set[String],
    getIndexedModuleNames: IndexedModule => Set[String],
  )(memberName: String): Option[NamePath] = {
    if (getNames(this).contains(memberName)) {
      Some(modulePath.append(memberName))
    } else {
      importMap
        .get(memberName)
        .orElse(parent.flatMap(_.getPath(getNames, getIndexedModuleNames)(memberName)))
        .orElse(globalModuleIndex.findPath(getIndexedModuleNames)(modulePath, memberName))
    }
  }
}

object LexicalModule {
  /**
    * An import map can immediately resolve the name paths for all imported simple names.
    */
  type ImportMap = Map[String, NamePath]
}
