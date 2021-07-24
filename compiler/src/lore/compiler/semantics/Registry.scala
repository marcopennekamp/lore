package lore.compiler.semantics

import lore.compiler.core.Position
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.Registry.MultiFunctionNotFound
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.scopes.{Binding, BindingScope, ImmutableTypeScope, TypeScope}
import lore.compiler.semantics.structures.{StructConstructorDefinition, SchemaDefinition}
import lore.compiler.types.{DeclaredType, DeclaredTypeHierarchy, NamedSchema}
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

  val declaredTypeHierarchy = new DeclaredTypeHierarchy(types.values.toVector.filterType[DeclaredType])

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
    * Gets the constructor of the struct with the given name.
    */
  def getStructConstructor(name: String): Option[StructConstructorDefinition] = typeScope.getStructSchema(name).map(_.definition.constructor)

}

object Registry {
  type Types = Map[String, NamedSchema]
  type TypeResolutionOrder = Vector[String]
  type TypeDefinitions = Map[String, SchemaDefinition]
  type MultiFunctions = Map[String, MultiFunctionDefinition]

  case class MultiFunctionNotFound(name: String, override val position: Position) extends Feedback.Error(position) {
    override def message = s"The multi-function $name does not exist in the current scope."
  }
}
