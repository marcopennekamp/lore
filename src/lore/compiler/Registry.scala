package lore.compiler

import lore.ast.{Node, TypeExprNode}
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
    // TODO: Build a declared type hierarchy graph that can be used to resolve sets of subtypes of a given declared type.
  }

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
    * Get a type definition from the registry, for example when looking for a superclass.
    */
  def getTypeDefinition(name: String, associatedNode: Node)(implicit fragment: Fragment): C[DeclaredTypeDefinition] = {
    // TODO: Do we even need this? We could just get a type and through it its definition. For example, we could
    //       add a function getClassType which also ensures that the type is a class type. If not, we can throw
    //       an ClassTypeExpected error.
    typeDefinitions.get(name) match {
      case None => Compilation.fail(Error.TypeNotFound(name, associatedNode))
      case Some(definition) => Compilation.succeed(definition)
    }
  }

  /**
    * Returns all type definitions accessible via an immutable map view.
    */
  def getTypeDefinitions: MapView[String, DeclaredTypeDefinition] = typeDefinitions.view

  // TODO: Also introduce getClassDefinition and getLabelDefinition. Use require/isInstanceOf and asInstanceOf.

  /**
    * Resolves a declared type with the given name.
    *
    * @param associatedNode The node where the type name occurs, to be used for error building.
    */
  def resolveType(name: String, associatedNode: Node)(implicit fragment: Fragment): C[Type] = {
    // TODO: We should rename this to getType or something similar.
    types.get(name) match {
      case None => Compilation.fail(Error.TypeNotFound(name, associatedNode))
      case Some(tpe) => Compilation.succeed(tpe)
    }
  }

  /**
    * Whether a type with the name `name` has been registered.
    */
  def hasType(name: String): Boolean = types.contains(name)

  /**
    * Resolves the declared supertype with the given name.
    */
  def resolveSupertype(maybeName: Option[String], associatedNode: TypeExprNode)(implicit fragment: Fragment): C[Option[Type]] = {
    maybeName.map(name => resolveType(name, associatedNode)).toCompiledOption
  }

  /* def addFunction(function: LoreFunction): Unit = {
    val multiFunction = multiFunctions.getOrElse(function.name, MultiFunction(function.name, Set()))
    multiFunctions.put(function.name, MultiFunction(function.name, Set(function) ++ multiFunction.functions))
  } */

}
