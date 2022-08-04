package lore.compiler.semantics.scopes

import lore.compiler.semantics.bindings.{StructConstructorBinding, StructObjectBinding, TermBinding}
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.semantics.{NamePath, Registry}

/**
  * A term scope backed by the registry and a local module for name resolution.
  *
  * The term scope does not contain plain struct constructors, even for constant schemas. [[StructConstructorBinding]]
  * is used in all cases. Objects are represented by [[StructObjectBinding]].
  */
case class LocalModuleTermScope(registry: Registry, localModule: LocalModule) extends TermScope {
  override protected def local(name: String): Option[TermBinding] = {
    localModule.terms.getAccessibleMembers(name).map { multiReference =>
      multiReference.singleBindingOption.getOrElse {
        // TODO (multi-import): Deal with multi-referable multi-functions.
        throw new UnsupportedOperationException(s"Not yet implemented. Bindings: ${multiReference.bindings}")
      }
    }
  }

  override def global(absolutePath: NamePath): Option[TermBinding] = {
    registry
      .getModule(absolutePath.parentOrEmpty)
      .flatMap(_.terms.get(absolutePath.simpleName))
  }
}
