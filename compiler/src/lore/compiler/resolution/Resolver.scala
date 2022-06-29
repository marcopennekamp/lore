package lore.compiler.resolution

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.semantics.scopes.{ImmutableTypeScope, TermScope, TypeScope}
import lore.compiler.syntax.DeclNode
import lore.compiler.types.TypeVariable

object Resolver {

  def withRegistryScopes[R](localModule: LocalModule)(f: TypeScope => TermScope => R)(
    implicit registry: Registry,
  ): R = {
    f(registry.getTypeScope(localModule))(registry.getTermScope(localModule))
  }

  def withTypeParameters[R](
    localModule: LocalModule,
    typeParameterNodes: Vector[DeclNode.TypeVariableNode],
  )(
    f: TypeScope => TermScope => Vector[TypeVariable] => R,
  )(implicit registry: Registry, reporter: Reporter): R = {
    withRegistryScopes(localModule) {
      typeScope => implicit termScope =>
        val typeParameters = TypeVariableResolver.resolve(typeParameterNodes, typeScope)
        val typeParameterScope: TypeScope = ImmutableTypeScope.from(typeParameters, typeScope)
        f(typeParameterScope)(termScope)(typeParameters)
    }
  }

}
