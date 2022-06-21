package lore.compiler.semantics.modules

import lore.compiler.semantics.{BindingKind, NamePath}

/**
  * The global module index manages all global modules and offers access to them via name paths.
  */
class GlobalModuleIndex {

  private var index: Map[NamePath, GlobalModule] = Map.empty

  // A global module index should always have its root module set. This simplifies various parts of resolution.
  getOrCreateModule(NamePath.empty)

  /**
    * The root module contains all members which aren't declared as part of a module themselves.
    */
  val root: GlobalModule = {
    index.getOrElse(NamePath.empty, throw new IllegalStateException("The root module must exist in a global module."))
  }

  /**
    * Returns all global modules.
    */
  def modules: Iterable[GlobalModule] = index.values

  /**
    * Gets the global module with the given name, if it exists.
    */
  def getModule(modulePath: NamePath): Option[GlobalModule] = index.get(modulePath)

  /**
    * Gets the global module with the given name if it exists, or creates a new one.
    */
  def getOrCreateModule(modulePath: NamePath): GlobalModule = this.synchronized {
    getModule(modulePath) match {
      case Some(globalModule) => globalModule
      case None =>
        val globalModule = new GlobalModule(modulePath)
        index = index.updated(modulePath, globalModule)
        globalModule
    }
  }

  /**
    * Whether the index has a binding with the exact name path.
    */
  def has(name: NamePath, memberKind: ModuleMemberKind): Boolean = {
    index
      .get(name.parentOrEmpty)
      .exists(m => m.members(memberKind).has(name.simpleName))
  }

  /**
    * Whether the index has a binding with the exact name path.
    */
  def has(name: NamePath): Boolean = {
    has(name, ModuleMemberKind.Type) || has(name, ModuleMemberKind.Term)
  }

  /**
    * Adds the binding `name` to the proper global module directly. This should be used for predefined members which
    * don't have a corresponding definition in some Lore fragment, such as the basic types.
    */
  def addMember(name: NamePath, bindingKind: BindingKind): Unit = this.synchronized {
    getOrCreateModule(name.parentOrEmpty)
      .members(bindingKind.toModuleMemberKind)
      .add(name.simpleName, bindingKind, None)
  }

}
