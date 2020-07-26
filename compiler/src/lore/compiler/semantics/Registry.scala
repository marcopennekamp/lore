package lore.compiler.semantics

import lore.compiler.core.Compilation.C
import lore.compiler.core.{Compilation, CompilationException, Error, Position}
import lore.compiler.semantics.Registry.{ConstructorNotFound, ExactFunctionNotFound, MultiFunctionNotFound, TypeNotFound}
import lore.compiler.semantics.functions.{ConstructorDefinition, FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.semantics.structures._
import lore.compiler.types._

import scala.collection.{MapView, mutable}

/**
  * The Registry holds all Definitions and Types known to the compiler.
  */
class Registry {
  /**
    * The list of named types declared in the whole project, including predefined types such as Int and Real.
    */
  private val types = mutable.HashMap[String, NamedType](Type.predefinedTypes.toList: _*)
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
  def resolveType(name: String)(implicit position: Position): C[NamedType] = {
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
    override protected def unknownEntry(name: String)(implicit position: Position): Error = TypeNotFound(name, position)
  }

  /**
    * Registers the given type definition. Also registers its type automatically.
    */
  def registerTypeDefinition(definition: DeclaredTypeDefinition): Unit = {
    // We don't have to check whether the type is already defined because registerType already throws a runtime
    // exception if it is.
    registerType(definition.name, definition.tpe)
    assert(!typeDefinitions.contains(definition.name))
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
  def resolveMultiFunction(name: String, position: Position): C[MultiFunctionDefinition] = {
    getMultiFunction(name) match {
      case None => Compilation.fail(MultiFunctionNotFound(name, position))
      case Some(mf) => Compilation.succeed(mf)
    }
  }

  /**
    * Gets an exact function with the given name and parameter types. If it cannot be found, the operation fails
    * with a compilation error.
    */
  def resolveExactFunction(name: String, types: List[Type], position: Position): C[FunctionDefinition] = {
    resolveMultiFunction(name, position).flatMap { mf =>
      mf.exact(ProductType(types)) match {
        case None => Compilation.fail(ExactFunctionNotFound(name, types, position))
        case Some(f) => Compilation.succeed(f)
      }
    }
  }

  /**
    * Resolves a constructor of a class with the given name.
    */
  def resolveConstructor(className: String, qualifier: Option[String], position: Position): C[ConstructorDefinition] = {
    getType(className).filter(_.isInstanceOf[ClassType]).map(_.asInstanceOf[ClassType]) match {
      case None => Compilation.fail(TypeNotFound(className, position))
      case Some(tpe) => resolveConstructor(tpe.definition, qualifier, position)
    }
  }

  /**
    * Resolves a constructor of the given class.
    */
  def resolveConstructor(definition: ClassDefinition, qualifier: Option[String], position: Position): C[ConstructorDefinition] = {
    val constructorName = qualifier.getOrElse(definition.name)
    definition.getConstructor(constructorName) match {
      case None => Compilation.fail(ConstructorNotFound(definition.name, qualifier, position))
      case Some(constructor) => Compilation.succeed(constructor)
    }
  }
}

object Registry {
  case class TypeNotFound(name: String, pos: Position) extends Error(pos) {
    override def message = s"The type $name does not exist in the current scope."
  }

  case class MultiFunctionNotFound(name: String, pos: Position) extends Error(pos) {
    override def message = s"The multi-function $name does not exist in the current scope."
  }

  case class ExactFunctionNotFound(name: String, types: List[Type], pos: Position) extends Error(pos) {
    override def message = s"The exact function $name[${types.mkString(", ")}] does not exist in the current scope."
  }

  case class ConstructorNotFound(typeName: String, qualifier: Option[String], pos: Position) extends Error(pos) {
    override def message = s"The constructor $typeName${qualifier.mkString(".")} does not exist in the class $typeName."
  }
}
