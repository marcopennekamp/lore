package lore.compiler.semantics

import lore.compiler.core.Position
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.Registry.MultiFunctionNotFound
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.scopes.{Binding, BindingScope, ImmutableTypeScope, TypeScope}
import lore.compiler.semantics.structures.{SchemaDefinition, StructConstructor}
import lore.compiler.types.{DeclaredSchema, DeclaredTypeHierarchy, NamedSchema, StructType}
import lore.compiler.utils.CollectionExtensions.{OptionExtension, VectorExtension}

/**
  * The Registry represents the global scope of type and multi-function definitions.
  */
case class Registry(
  types: Registry.Types,
  typeResolutionOrder: Registry.TypeResolutionOrder,
  typeDefinitions: Registry.TypeDefinitions,
  multiFunctions: Registry.MultiFunctions,
) {

  val declaredTypeHierarchy = new DeclaredTypeHierarchy(types.values.toVector.filterType[DeclaredSchema])

  /**
    * All schemas in their proper order of resolution. Excludes predefined types.
    */
  val schemasInOrder: Vector[(String, NamedSchema)] = typeResolutionOrder.map(name => (name, types(name)))

  /**
    * The global type scope backed by the registry.
    */
  val typeScope: TypeScope = ImmutableTypeScope(types, None)

  /**
    * The global binding scope backed by the registry, containing multi-functions and struct constructors.
    */
  val bindingScope: BindingScope = new BindingScope {
    override protected def local(name: String): Option[Binding] = multiFunctions.get(name).orElse(getStructConstructor(name))
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
    * Gets the constructor of the struct with the given name. Type aliases are also supported directly.
    */
  def getStructConstructor(name: String): Option[StructConstructor] = {
    // TODO (schemas): Using `representative` on the struct schema is a temporary fix that only works for struct types
    //                 without type parameters.
    // TODO (schemas): This doesn't yet cover the case where a parameterized alias is used to construct a struct.
    typeScope.getStructSchema(name).map(_.representative.constructor).orElse {
      typeScope.getAliasSchema(name)
        .filter(_.isConstant)
        .map(_.representative)
        .filterType[StructType]
        .map(_.constructor)
    }
  }

}

object Registry {
  type Types = Map[String, NamedSchema] // TODO (schemas): Rename to `Schemas`?
  type TypeResolutionOrder = Vector[String] // TODO (schemas): Rename to `SchemaResolutionOrder`?
  type TypeDefinitions = Map[String, SchemaDefinition] // TODO (schemas): Rename to `SchemaDefinitions`?
  type MultiFunctions = Map[String, MultiFunctionDefinition]

  case class MultiFunctionNotFound(name: String, override val position: Position) extends Feedback.Error(position) {
    override def message = s"The multi-function $name does not exist in the current scope."
  }
}
