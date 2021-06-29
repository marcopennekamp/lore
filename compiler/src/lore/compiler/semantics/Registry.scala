package lore.compiler.semantics

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, Position}
import lore.compiler.feedback.Feedback
import lore.compiler.semantics.Registry.MultiFunctionNotFound
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.scopes.{Binding, BindingScope, ImmutableTypeScope, TypeScope}
import lore.compiler.semantics.structures.{DeclaredTypeDefinition, StructConstructorDefinition}
import lore.compiler.types.{DeclaredType, DeclaredTypeHierarchy, Type}
import lore.compiler.utils.CollectionExtensions.VectorExtension

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
    * All types in their proper order of resolution. Excludes predefined types.
    */
  val typesInOrder: Vector[(String, Type)] = typeResolutionOrder.map(name => (name, types(name)))

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
    * Gets a multi-function with the given name. If it cannot be found, the operation fails with a compilation error.
    */
  def resolveMultiFunction(name: String, position: Position): Compilation[MultiFunctionDefinition] = {
    multiFunctions.get(name) match {
      case None => Compilation.fail(MultiFunctionNotFound(name, position))
      case Some(mf) => mf.compiled
    }
  }

  /**
    * Gets the constructor of the struct with the given name.
    */
  def getStructConstructor(name: String): Option[StructConstructorDefinition] = typeScope.getStructType(name).map(_.definition.constructor)

}

object Registry {
  type Types = Map[String, Type]
  type TypeResolutionOrder = Vector[String]
  type TypeDefinitions = Map[String, DeclaredTypeDefinition]
  type MultiFunctions = Map[String, MultiFunctionDefinition]

  case class MultiFunctionNotFound(name: String, override val position: Position) extends Feedback.Error(position) {
    override def message = s"The multi-function $name does not exist in the current scope."
  }
}
