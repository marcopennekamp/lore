package lore.compiler.semantics.scopes

import lore.compiler.semantics.modules.LocalModule
import lore.compiler.semantics.{NameKind, NamePath, Registry}
import lore.compiler.types.NamedSchema

/**
  * A type scope backed by the registry and a local module for name resolution.
  */
case class LocalModuleTypeScope(
  localModule: LocalModule,
  types: Registry.Types,
) extends TypeScope {

  override protected def local(name: String): Option[NamedSchema] = {
    localModule.getPath(name, NameKind.Type).flatMap {
      namePath => types.schemas.get(namePath)
    }
  }

  override protected def resolveAbsolute(absolutePath: NamePath): Option[NamedSchema] = {
    types.schemas.get(absolutePath)
  }

}
