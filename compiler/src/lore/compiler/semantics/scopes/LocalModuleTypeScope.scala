package lore.compiler.semantics.scopes

import lore.compiler.semantics.{NamePath, Registry}
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.types.NamedSchema

/**
  * A type scope backed by the registry, and a local module for name resolution.
  */
case class LocalModuleTypeScope(localModule: LocalModule, registry: Registry) extends TypeScope {
  override protected def local(name: String): Option[NamedSchema] = {
    // We can use `_.singleMember` here because types aren't multi-referable.
    localModule.types
      .getAccessibleMembers(name)
      .flatMap(_.singleMember.schema.toOption)
  }

  override def global(absolutePath: NamePath): Option[NamedSchema] = {
    registry
      .getModule(absolutePath.parentOrEmpty)
      .flatMap(_.types.get(absolutePath.simpleName))
      .flatMap(_.schema.toOption)
  }
}
