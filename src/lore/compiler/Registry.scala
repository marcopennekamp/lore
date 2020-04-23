package lore.compiler

import lore.ast.Node
import lore.compiler.Compilation.C
import lore.compiler.feedback.{Position, Error}
import lore.definitions.{DeclaredTypeDefinition, MultiFunctionDefinition}
import lore.types._

import scala.collection.{MapView, mutable}

/**
  * The Registry holds all Definitions and Types known to the compiler.
  */
class Registry {
  /**
    * The list of types declared in the whole project, including predefined types such as Int and Real.
    */
  private val types = mutable.HashMap[String, Type](Type.predefinedTypes.toList:_*)
  private val typeDefinitions = mutable.HashMap[String, DeclaredTypeDefinition]()
  private val multiFunctions = mutable.HashMap[String, MultiFunctionDefinition]()
  val declaredTypeHierarchy = new DeclaredTypeHierarchy()

  /**
    * Registers a type with the specific name.
    */
  def registerType(name: String, tpe: Type): Unit = {
    if (types.contains(name)) {
      // We throw a runtime exception instead of returning a compilation error, because at this point, if we register
      // a type that is already registered, it is a COMPILER BUG and not a user error!
      throw new RuntimeException(
        s"The type $name has already been registered. This is likely a compiler bug."
      )
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
  def getTypes: MapView[String, Type] = types.view

  /**
    * Whether a type with the name `name` has been registered.
    */
  def hasType(name: String): Boolean = types.contains(name)

  /**
    * Searches for a type with the given name.
    */
  def getType(name: String): Option[Type] = types.get(name)

  /**
    * Gets a named type with the given name. If the type cannot be found, the operation fails with a compilation error.
    *
    * @param associatedNode The node where the type name occurs, to be used for error building.
    */
  def resolveType(name: String, associatedNode: Node)(implicit fragment: Fragment): C[Type] = {
    getType(name) match {
      case None => Compilation.fail(Error.TypeNotFound(name, associatedNode))
      case Some(tpe) => Compilation.succeed(tpe)
    }
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
      // We throw a runtime exception instead of returning a compilation error, because it is always a compiler bug
      // if a multi-function with the same name is registered twice.
      throw new RuntimeException(
        s"The multi-function ${multiFunction.name} has already been registered. This is likely a compiler bug."
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
      case None => Compilation.fail(Error.MultiFunctionNotFound(name, position))
      case Some(mf) => Compilation.succeed(mf)
    }
  }
}
