package lore.compiler.semantics.scopes

import lore.compiler.semantics.modules.LocalModule
import lore.compiler.semantics.{NamePath, Registry}
import lore.compiler.types.NamedSchema

/**
  * A type scope backed by the registry and a local module for name resolution.
  */
case class LocalModuleTypeScope(
  localModule: LocalModule,
  types: Registry.Types,
) extends TypeScope {
  override protected def local(name: String): Option[NamedSchema] = {
    // We can use `_.singlePath` here because types aren't multi-referable.
    localModule.types.getAbsolutePaths(name).map(_.singlePath).flatMap(global)
  }

  override def global(absolutePath: NamePath): Option[NamedSchema] = types.schemas.get(absolutePath)
}
