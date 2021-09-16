package lore.compiler.semantics.scopes

import lore.compiler.semantics.modules.LocalModule
import lore.compiler.semantics.{NameKind, Registry}

/**
  * A binding scope backed by the registry and a local module for name resolution. It contains modules,
  * global variables, multi-functions, struct bindings, and objects.
  *
  * The binding scope does not contain plain struct constructors, even for constant schemas. A
  * [[StructConstructorBinding]] is used in all cases. Objects are represented by [[StructObjectBinding]].
  */
case class LocalModuleBindingScope(
  localModule: LocalModule,
  bindings: Registry.Bindings,
) extends BindingScope {

  override protected def local(name: String): Option[Binding] = {
    localModule.getPath(name, NameKind.Binding).flatMap { path =>
      bindings.globalVariables.get(path)
        .orElse(bindings.multiFunctions.get(path))
        // Struct bindings have higher priority than modules, because companion modules shouldn't hide struct bindings,
        // but a struct binding may specify that it has a companion module.
        .orElse(bindings.structBindings.get(path))
        .orElse(bindings.modules.get(path))
    }
  }

}
