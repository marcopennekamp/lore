package lore.compiler.semantics.scopes

import lore.compiler.semantics.bindings.TypeBinding
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.semantics.{NamePath, Registry}

/**
  * A type scope backed by the registry and a local module for name resolution.
  */
case class LocalModuleTypeScope(registry: Registry, localModule: LocalModule) extends TypeScope {
  override protected def local(name: String): Option[TypeBinding] = {
    // We can use `_.singleBinding` here because types aren't multi-referable.
    localModule.types
      .getAccessibleMembers(name)
      .map(_.singleBinding)
  }

  override def global(absolutePath: NamePath): Option[TypeBinding] = {
    registry
      .getModule(absolutePath.parentOrEmpty)
      .flatMap(_.types.get(absolutePath.simpleName))
  }
}
