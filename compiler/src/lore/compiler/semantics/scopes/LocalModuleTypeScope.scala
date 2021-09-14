package lore.compiler.semantics.scopes

import lore.compiler.semantics.{NameKind, NamePath}
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.types.NamedSchema

/**
  * A LocalModuleTypeScope implements the lookup and precedence criteria described in [[LocalModule]] for types.
  *
  * TODO (modules): If we go back to a flat Registry without ModuleDefinitions, `schemas` would become a Registry
  *                 `typeScope.get(namePath)`.
  */
case class LocalModuleTypeScope(localModule: LocalModule, schemas: Map[NamePath, NamedSchema]) extends TypeScope {
  override protected def local(name: String): Option[NamedSchema] = {
    localModule.getPath(name, NameKind.Type).flatMap {
      namePath => schemas.get(namePath)
    }
  }

  override protected def add(name: String, entry: NamedSchema): Unit = ???
}
