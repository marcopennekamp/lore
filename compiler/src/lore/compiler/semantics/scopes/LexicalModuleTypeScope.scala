package lore.compiler.semantics.scopes

import lore.compiler.semantics.NamePath
import lore.compiler.semantics.modules.LexicalModule
import lore.compiler.types.NamedSchema

/**
  * A lexical module type scope implements the lookup and precedence criteria described in [[LexicalModule]].
  *
  * TODO (modules): If we go back to a flat Registry without ModuleDefinitions, `schemas` would become a Registry
  *                 `typeScope.get(namePath)`.
  */
case class LexicalModuleTypeScope(lexicalModule: LexicalModule, schemas: Map[NamePath, NamedSchema]) extends TypeScope {
  override protected def local(name: String): Option[NamedSchema] = {
    lexicalModule.getTypePath(name).flatMap {
      namePath => schemas.get(namePath)
    }
  }

  override protected def add(name: String, entry: NamedSchema): Unit = ???
}
