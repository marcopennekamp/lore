package lore.compiler.semantics

import lore.compiler.core.Position
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.Registry.MultiFunctionNotFound
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.scopes.{Binding, BindingScope, ImmutableTypeScope, StructBinding, TypeScope}
import lore.compiler.semantics.structures.SchemaDefinition
import lore.compiler.types.{DeclaredSchema, DeclaredTypeHierarchy, NamedSchema, StructType}
import lore.compiler.utils.CollectionExtensions.{OptionExtension, VectorExtension}

/**
  * The Registry represents the global scope of type and multi-function definitions.
  */
case class Registry(
  schemas: Registry.Schemas,
  schemaResolutionOrder: Registry.SchemaResolutionOrder,
  schemaDefinitions: Registry.SchemaDefinitions,
  multiFunctions: Registry.MultiFunctions,
) {

  val declaredTypeHierarchy = new DeclaredTypeHierarchy(schemas.values.toVector.filterType[DeclaredSchema])

  /**
    * All schemas in their proper order of resolution. Excludes predefined types.
    */
  val schemasInOrder: Vector[(String, NamedSchema)] = schemaResolutionOrder.map(name => (name, schemas(name)))

  /**
    * The global type scope backed by the registry.
    */
  val typeScope: TypeScope = ImmutableTypeScope(schemas, None)

  /**
    * The global binding scope backed by the registry, containing multi-functions and struct bindings.
    *
    * The binding scope does not contain plain struct constructors, even for constant schemas. A [[StructBinding]] is
    * used in all cases.
    */
  val bindingScope: BindingScope = new BindingScope {
    override protected def local(name: String): Option[Binding] = multiFunctions.get(name).orElse(getStructBinding(name))
    override protected def add(name: String, entry: Binding): Unit = {
      throw new UnsupportedOperationException(s"You may not add bindings to the Registry via its BindingScope interface. Name: $name. Binding: $entry.")
    }
  }

  /**
    * Gets a multi-function with the given name. An appropriate error is reported if it cannot be found.
    */
  def resolveMultiFunction(name: String, position: Position)(implicit reporter: Reporter): Option[MultiFunctionDefinition] = {
    multiFunctions.get(name).ifEmpty(reporter.error(MultiFunctionNotFound(name, position)))
  }

  /**
    * Gets a struct binding from a struct schema with the given name. If the name refers to a type alias that
    * represents a struct type, the struct binding will be able to instantiate the correct struct type given the type
    * alias's type parameters.
    */
  def getStructBinding(name: String): Option[StructBinding] = {
    typeScope.getStructSchema(name).map(schema => StructBinding(name, schema.parameters, schema.representative)).orElse {
      typeScope.getAliasSchema(name).flatMap { aliasSchema =>
        Some(aliasSchema.representative)
          .filterType[StructType]
          .map(StructBinding(name, aliasSchema.parameters, _))
      }
    }
  }

}

object Registry {
  type Schemas = Map[String, NamedSchema]
  type SchemaResolutionOrder = Vector[String]
  type SchemaDefinitions = Map[String, SchemaDefinition]
  type MultiFunctions = Map[String, MultiFunctionDefinition]

  case class MultiFunctionNotFound(name: String, override val position: Position) extends Feedback.Error(position) {
    override def message = s"The multi-function $name does not exist in the current scope."
  }
}
