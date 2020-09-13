package lore.compiler.semantics

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, CompilationException, Error, Position}
import lore.compiler.semantics.Registry.{ExactFunctionNotFound, MultiFunctionNotFound, TypeNotFound}
import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.semantics.structures._
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions._

import scala.collection.{MapView, mutable}

/**
  * The Registry holds all Definitions and Types known to the compiler.
  */
class Registry {
  /**
    * The list of named types declared in the whole project, including predefined types such as Int and Real.
    */
  private val types = mutable.HashMap[String, NamedType](Type.predefinedTypes.toList: _*)
  private var typeRegistrationOrder = Vector[String]()
  private val typeDefinitions = mutable.HashMap[String, DeclaredTypeDefinition]()
  private val multiFunctions = mutable.HashMap[String, MultiFunctionDefinition]()
  val declaredTypeHierarchy = new DeclaredTypeHierarchy()

  /**
    * Registers a type with the specific name.
    */
  def registerType(name: String, tpe: NamedType): Unit = {
    // At this point, a legitimate error should have been raised if a type name is not unique, so this is
    // a compiler error.
    if (types.contains(name)) {
      throw CompilationException(s"The type $name has already been registered.")
    }
    types.put(name, tpe)
    typeRegistrationOrder = typeRegistrationOrder :+ name

    // If this is a declared type, also register it in the declared type hierarchy.
    tpe match {
      case declaredType: DeclaredType => declaredTypeHierarchy.addType(declaredType)
      case _ =>
    }
  }

  /**
    * Returns all registered types, accessible via an immutable map view.
    */
  def getTypes: MapView[String, NamedType] = types.view

  /**
    * Returns all registered types in their order of registration.
    */
  def getTypesInOrder: Vector[NamedType] = typeRegistrationOrder.map(types(_))

  /**
    * Whether a type with the given name has been registered.
    */
  def hasType(name: String): Boolean = types.contains(name)

  /**
    * Searches for a type with the given name.
    */
  def getType(name: String): Option[NamedType] = types.get(name)

  /**
    * Gets a named type with the given name. If the type cannot be found, the operation fails with a compilation error.
    * The difference from getType is that this results in a compilation with a clear failure state.
    *
    * @param position The position where the type name occurs, to be used for error building.
    */
  def resolveType(name: String)(implicit position: Position): Compilation[NamedType] = {
    typeScope.resolve(name)
  }

  /**
    * The TypeScope view of the registry. This is strictly an immutable view.
    */
  val typeScope: TypeScope = new TypeScope {
    override protected def local(name: String): Option[NamedType] = getType(name)
    override protected def add(entry: NamedType): Unit = {
      throw new UnsupportedOperationException("You may not add types to the Registry via its TypeScope interface.")
    }
    override protected def unknownEntry(name: String)(implicit position: Position): Error = TypeNotFound(name)
  }

  /**
    * Searches for a struct type with the given name.
    */
  def getStructType(name: String): Option[StructType] = getType(name).filterType[StructType]

  /**
    * Searches for a trait type with the given name.
    */
  def getTraitType(name: String): Option[TraitType] = getType(name).filterType[TraitType]

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
  def resolveExactFunction(name: String, types: List[Type])(implicit position: Position): Compilation[FunctionDefinition] = {
    resolveMultiFunction(name).flatMap { mf =>
      mf.exact(ProductType(types)) match {
        case None => Compilation.fail(ExactFunctionNotFound(name, types))
        case Some(f) => f.compiled
      }
    }
  }
}

object Registry {
  case class TypeNotFound(name: String)(implicit position: Position) extends Error(position) {
    override def message = s"The type $name does not exist in the current scope."
  }

  case class MultiFunctionNotFound(name: String)(implicit position: Position) extends Error(position) {
    override def message = s"The multi-function $name does not exist in the current scope."
  }

  case class ExactFunctionNotFound(name: String, types: List[Type])(implicit position: Position) extends Error(position) {
    override def message = s"The exact function $name[${types.mkString(", ")}] does not exist in the current scope."
  }
}
