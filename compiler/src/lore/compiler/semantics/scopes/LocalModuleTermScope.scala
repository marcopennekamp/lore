package lore.compiler.semantics.scopes

import lore.compiler.core.CompilationException
import lore.compiler.semantics.bindings.{AmbiguousMultiFunction, StructConstructorBinding, StructObjectBinding, TermBinding}
import lore.compiler.semantics.definitions.BindingDefinitionKind
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.modules.{LocalModule, MultiReference}
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
        if (multiReference.definitionKind == BindingDefinitionKind.MultiFunction) {
          //AmbiguousMultiFunction(multiReference.asInstanceOf[MultiReference[MultiFunctionDefinition]])
          // TODO (multi-import): Temp simplification. Remove.
          multiReference.local.head
        } else {
          throw CompilationException("Cannot build an ambiguous term binding from a binding with definition kind" +
            s" ${multiReference.definitionKind}.")
        }
      }
    }
  }

  override def global(absolutePath: NamePath): Option[TermBinding] = {
    registry
      .getModule(absolutePath.parentOrEmpty)
      .flatMap(_.terms.get(absolutePath.simpleName))
  }
}
