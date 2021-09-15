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

  /**
    * Gets a [[StructConstructorBinding]] from a struct schema with the given name. If the name refers to a type alias that
    * represents a struct type, the struct binding will be able to instantiate the struct with the correct struct type
    * given the type alias's type parameters.
    *
    * Objects are represented by [[StructObjectBinding]].
    *
    * TODO (modules): This should be moved to the declaration resolver, where we'll generate a struct binding for each
    *                 struct schema and type alias.
    */
  /*private def getStructBinding(name: String): Option[Binding] = {
    def getByType(tpe: StructType, typeParameters: Vector[TypeVariable]): Binding = {
      if (tpe.schema.definition.isObject) {
        if (typeParameters.nonEmpty) {
          throw CompilationException(s"Objects cannot have type parameters. Violated by object ${tpe.name}.")
        }
        StructObjectBinding(name, tpe)
      } else {
        StructConstructorBinding(name, typeParameters, tpe)
      }
    }

    typeScope.getStructSchema(name).map(schema => getByType(schema.representative, schema.parameters)).orElse {
      typeScope.getAliasSchema(name).flatMap { aliasSchema =>
        Some(aliasSchema.representative)
          .filterType[StructType]
          .map(getByType(_, aliasSchema.parameters))
      }
    }
  }*/

}
