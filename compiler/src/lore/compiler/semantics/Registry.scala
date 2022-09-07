package lore.compiler.semantics

import lore.compiler.semantics.core.CoreDefinitions
import lore.compiler.semantics.definitions.TypeDefinition
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.modules.{GlobalModule, LocalModule}
import lore.compiler.semantics.scopes.{LocalModuleTermScope, LocalModuleTypeScope}
import lore.compiler.types.{DeclaredTypeHierarchy, NamedSchema}
import lore.compiler.utils.Once

/**
  * The Registry manages all global modules and offers access to global modules and module members via name paths.
  */
class Registry {

  private var moduleIndex: Map[NamePath, GlobalModule] = Map.empty
  val declaredTypeHierarchy: Once[DeclaredTypeHierarchy] = new Once
  val coreDefinitions: Once[CoreDefinitions] = new Once

  /**
    * The root module contains all members which aren't declared as part of a module themselves. It is immediately
    * created with the Registry, which simplifies various resolution steps.
    */
  val rootModule: GlobalModule = getOrCreateModule(NamePath.empty)

  /**
    * Returns all global modules.
    */
  def modules: Iterable[GlobalModule] = moduleIndex.values

  /**
    * Gets the global module with the given name, if it exists.
    */
  def getModule(modulePath: NamePath): Option[GlobalModule] = moduleIndex.get(modulePath)

  /**
    * Gets the global module with the given name if it exists, or creates a new one.
    */
  def getOrCreateModule(modulePath: NamePath): GlobalModule = this.synchronized {
    getModule(modulePath) match {
      case Some(globalModule) => globalModule
      case None =>
        val globalModule = new GlobalModule(modulePath)
        moduleIndex = moduleIndex.updated(modulePath, globalModule)
        globalModule
    }
  }

  /**
    * Whether the index has a binding with the exact name path.
    */
  def has(name: NamePath): Boolean = {
    moduleIndex
      .get(name.parentOrEmpty)
      .exists(module => module.types.has(name.simpleName) || module.terms.has(name.simpleName))
  }

  /**
    * Returns the type definition `name`. If the member doesn't exist, `None` is returned.
    */
  def getType(name: NamePath): Option[TypeDefinition] = {
    getModule(name.parentOrEmpty).flatMap(_.types.get(name.simpleName))
  }

  /**
    * Returns the multi-function `name`. If the member doesn't exist or isn't a multi-function, `None` is returned.
    */
  def getMultiFunction(name: NamePath): Option[MultiFunctionDefinition] = {
    getModule(name.parentOrEmpty).flatMap(_.getMultiFunction(name.simpleName))
  }

  /**
    * Creates a type scope that represents the Registry. Name resolution requires the presence of a local module.
    */
  def getTypeScope(localModule: LocalModule): LocalModuleTypeScope = LocalModuleTypeScope(this, localModule)

  /**
    * Creates a term scope that represents the Registry. Name resolution requires the presence of a local module.
    */
  def getTermScope(localModule: LocalModule): LocalModuleTermScope = LocalModuleTermScope(this, localModule)

}
