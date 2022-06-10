package lore.compiler.semantics.scopes

import lore.compiler.semantics.bindings.{StructConstructorBinding, StructObjectBinding, TermBinding}
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.semantics.{BindingKind, NamePath, Registry}

/**
  * A term scope backed by the registry and a local module for name resolution. It contains modules, global variables,
  * multi-functions, struct bindings, and objects.
  *
  * The term scope does not contain plain struct constructors, even for constant schemas. [[StructConstructorBinding]]
  * is used in all cases. Objects are represented by [[StructObjectBinding]].
  */
case class LocalModuleTermScope(
  localModule: LocalModule,
  terms: Registry.Terms,
) extends TermScope {
  override protected def local(name: String): Option[TermBinding] = {
    localModule.getAbsolutePath(name, BindingKind.Term).flatMap(global)
  }

  override def global(absolutePath: NamePath): Option[TermBinding] = {
    terms.globalVariables.get(absolutePath)
      .orElse(terms.multiFunctions.get(absolutePath))
      // Struct bindings have higher priority than modules, because companion modules shouldn't hide struct bindings,
      // but a struct binding may specify that it has a companion module.
      .orElse(terms.structBindings.get(absolutePath))
      .orElse(terms.modules.get(absolutePath))
  }
}
