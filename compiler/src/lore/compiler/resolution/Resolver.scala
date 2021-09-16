package lore.compiler.resolution

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.semantics.scopes.{BindingScope, ImmutableTypeScope, TypeScope}
import lore.compiler.syntax.DeclNode
import lore.compiler.types.TypeVariable

object Resolver {

  def withRegistryScopes[R](localModule: LocalModule)(
    f: TypeScope => BindingScope => R,
  )(implicit types: Registry.Types, bindings: Registry.Bindings): R = {
    f(types.scope(localModule))(bindings.scope(localModule))
  }

  def withTypeParameters[R](localModule: LocalModule, typeParameterNodes: Vector[DeclNode.TypeVariableNode])(
    f: TypeScope => BindingScope => Vector[TypeVariable] => R,
  )(implicit types: Registry.Types, bindings: Registry.Bindings, reporter: Reporter): R = {
    withRegistryScopes(localModule) {
      typeScope => implicit bindingScope =>
        val typeParameters = TypeVariableResolver.resolve(typeParameterNodes, typeScope)
        val typeParameterScope: TypeScope = ImmutableTypeScope.from(typeParameters, typeScope)
        f(typeParameterScope)(bindingScope)(typeParameters)
    }
  }

}
