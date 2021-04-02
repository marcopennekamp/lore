package lore.compiler.semantics

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, CompilationException, Error, Position}
import lore.compiler.semantics.Registry.{ExactFunctionNotFound, MultiFunctionNotFound, RegistryVariableNotFound, TypeNotFound}
import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.semantics.scopes.{TypeScope, Variable, VariableScope}
import lore.compiler.semantics.structures._
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions._

import scala.collection.{MapView, mutable}

/**
  * The Registry holds all definitions and types known to the compiler.
  */
class Registry {

  // TODO: Combine aspects of the registry and declaration resolver into a function-ish where mutations are only
  //       localized. There is no reason why the registry should be mutable. The registry should not offer register*
  //       functions.

  /**
    * The list of named types declared in the whole project, including predefined types such as Int and Real and
    * type aliases.
    */
  private val types = mutable.HashMap[String, Type](Type.predefinedTypes.toVector: _*)
  private var typeDeclarationsInOrder = Vector[(String, Type)]()
  private val typeDefinitions = mutable.HashMap[String, DeclaredTypeDefinition]()
  private val multiFunctions = mutable.HashMap[String, MultiFunctionDefinition]()
  val declaredTypeHierarchy = new DeclaredTypeHierarchy()

  /**
    * Registers a type with the specific name.
    */
  def registerType(name: String, tpe: Type): Unit = {
    // At this point, a legitimate error should have been raised if a type name is not unique, so this is
    // a compiler error.
    if (types.contains(name)) {
      throw CompilationException(s"The type $name has already been registered.")
    }
    types.put(name, tpe)
    typeDeclarationsInOrder = typeDeclarationsInOrder :+ (name, tpe)

    // If this is a declared type, also register it in the declared type hierarchy.
    // TODO: Can't we build this hierarchy after all types have been registered? This would reduce the complexity of
    //       this function.
    tpe match {
      case declaredType: DeclaredType => declaredTypeHierarchy.addType(declaredType)
      case _ =>
    }
  }

  /**
    * Returns all registered types, accessible via an immutable map view. Includes predefined types.
    */
  def getTypes: MapView[String, Type] = types.view

  /**
    * Returns all types in their order of registration. Excludes predefined types.
    */
  def getTypeDeclarationsInOrder: Vector[(String, Type)] = typeDeclarationsInOrder

  /**
    * Whether a type with the given name has been registered.
    */
  def hasType(name: String): Boolean = types.contains(name)

  /**
    * Searches for a type with the given name.
    */
  def getType(name: String): Option[Type] = types.get(name)

  /**
    * Searches for a struct type with the given name.
    */
  def getStructType(name: String): Option[StructType] = getType(name).filterType[StructType]

  /**
    * Searches for a trait type with the given name.
    */
  def getTraitType(name: String): Option[TraitType] = getType(name).filterType[TraitType]

  /**
    * Gets a type with the given name. If the type cannot be found, the operation fails with a compilation error.
    * The difference from getType is that this results in a compilation with a clear failure state.
    *
    * @param position The position where the type name occurs, to be used for error building.
    */
  def resolveType(name: String)(implicit position: Position): Compilation[Type] = {
    typeScope.resolve(name)
  }

  /**
    * The global type scope backed by the registry. This is strictly an immutable view.
    */
  val typeScope: TypeScope = new TypeScope {
    override protected def local(name: String): Option[Type] = getType(name)
    override protected def add(name: String, entry: Type): Unit = {
      throw new UnsupportedOperationException(s"You may not add types to the Registry via its TypeScope interface. Name: $name. Type: $entry.")
    }
    override protected def unknownEntry(name: String)(implicit position: Position): Error = TypeNotFound(name)
  }

  /**
    * Registers the given type definition.
    */
  def registerTypeDefinition(definition: DeclaredTypeDefinition): Unit = {
    // Because types are resolved before definitions, at this point a type for this definition should have been registered.
    if (getType(definition.name).isEmpty) {
      throw CompilationException(s"A type for the declared type ${definition.name} should have been registered by now.")
    }

    if (typeDefinitions.contains(definition.name)) {
      // TODO: Do we actually check for duplicate definitions beforehand?
      throw CompilationException(s"A type definition with the name ${definition.name} has been registered already.")
    }
    typeDefinitions.put(definition.name, definition)
  }

  /**
    * Returns all registered type definitions, accessible via an immutable map view.
    */
  def getTypeDefinitions: MapView[String, DeclaredTypeDefinition] = typeDefinitions.view

  /**
    * Registers a multi-function.
    */
  def registerMultiFunction(multiFunction: MultiFunctionDefinition): Unit = {
    if (multiFunctions.contains(multiFunction.name)) {
      throw CompilationException(
        s"The multi-function ${multiFunction.name} has already been registered."
      )
    }
    multiFunctions.put(multiFunction.name, multiFunction)
  }

  /**
    * Returns all registered multi-functions, accessible via an immutable map view.
    */
  def getMultiFunctions: MapView[String, MultiFunctionDefinition] = multiFunctions.view

  /**
    * Searches for a multi-function with the given name.
    */
  def getMultiFunction(name: String): Option[MultiFunctionDefinition] = multiFunctions.get(name)

  /**
    * Gets a multi-function with the given name. If it cannot be found, the operation fails with a compilation error.
    */
  def resolveMultiFunction(name: String)(implicit position: Position): Compilation[MultiFunctionDefinition] = {
    getMultiFunction(name) match {
      case None => Compilation.fail(MultiFunctionNotFound(name))
      case Some(mf) => mf.compiled
    }
  }

  /**
    * Gets an exact function with the given name and parameter types. If it cannot be found, the operation fails
    * with a compilation error.
    */
  def resolveExactFunction(name: String, types: Vector[Type])(implicit position: Position): Compilation[FunctionDefinition] = {
    resolveMultiFunction(name).flatMap { mf =>
      mf.exact(ProductType(types)) match {
        case None => Compilation.fail(ExactFunctionNotFound(name, types))
        case Some(f) => f.compiled
      }
    }
  }

  def getStructConstructor(name: String): Option[StructConstructorDefinition] = getStructType(name).map(_.definition.constructor)

  /**
    * The global variable scope backed by the registry, containing multi-functions and struct constructors.
    */
  val variableScope: VariableScope = new VariableScope {
    override protected def local(name: String): Option[Variable] = getMultiFunction(name).orElse(getStructConstructor(name))
    override protected def add(name: String, entry: Variable): Unit = {
      throw new UnsupportedOperationException(s"You may not add variables to the Registry via its VariableScope interface. Name: $name. Variable: $entry.")
    }
    override protected def unknownEntry(name: String)(implicit position: Position): Error = RegistryVariableNotFound(name, position)
  }

}

object Registry {
  case class TypeNotFound(name: String)(implicit position: Position) extends Error(position) {
    override def message = s"The type $name does not exist in the current scope."
  }

  case class MultiFunctionNotFound(name: String)(implicit position: Position) extends Error(position) {
    override def message = s"The multi-function $name does not exist in the current scope."
  }

  case class ExactFunctionNotFound(name: String, types: Vector[Type])(implicit position: Position) extends Error(position) {
    override def message = s"The exact function $name[${types.mkString(", ")}] does not exist in the current scope."
  }

  case class RegistryVariableNotFound(name: String, override val position: Position) extends Error(position) {
    override def message = s"The multi-function or struct constructor $name does not exist in the current scope."
  }
}
