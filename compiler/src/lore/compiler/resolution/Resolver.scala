package lore.compiler.resolution

import lore.compiler.semantics.Registry
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.semantics.scopes.{TermScope, TypeScope}

object Resolver {

  def withRegistryScopes[R](localModule: LocalModule)(f: TypeScope => TermScope => R)(
    implicit registry: Registry,
  ): R = {
    f(registry.getTypeScope(localModule))(registry.getTermScope(localModule))
  }

}
