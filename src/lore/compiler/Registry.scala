package lore.compiler

import lore.ast.{Node, TypeExprNode}
import lore.definitions.DeclaredTypeDefinition
import lore.functions.MultiFunction
import lore.types._

import scala.collection.mutable

/**
  * The Registry holds all Definitions and Types known to the compiler.
  */
class Registry {

  /**
    * The list of types declared in the whole project, including predefined types such as Int and Real.
    */
  private val types = mutable.HashMap[String, Type](Type.predefinedTypes.toList:_*)
  private val typeDefinitions = mutable.HashMap[String, DeclaredTypeDefinition]()
  private val multiFunctions = mutable.HashMap[String, MultiFunction]()

  /**
    * Registers the given type definition. Also registers its type automatically.
    */
  def registerTypeDefinition(definition: DeclaredTypeDefinition): Unit = {
    typeDefinitions.put(definition.name, definition)
    registerType(definition.name, definition.tpe)
  }

  /**
    * Registers a type with the specific name.
    */
  def registerType(name: String, tpe: Type): Unit = {
    if (types.contains(name)) {
      // We throw a runtime exception instead of returning a compilation error, because at this point, if we register
      // a type that is already registered, it is a COMPILER BUG and not a user error!
      throw new RuntimeException(
        s"The type $name has already been registered. It should not happen at this point and is likely a compiler bug."
      )
    }
    types.put(name, tpe)
  }

  /**
    * Get a type definition from the registry, for example when looking for a superclass.
    */
  def getTypeDefinition(name: String, associatedNode: Node): C[DeclaredTypeDefinition] = {
    // TODO: Do we even need this? We could just get a type and through it its definition. For example, we could
    //       add a function getClassType which also ensures that the type is a class type. If not, we can throw
    //       an ClassTypeExpected error.
    typeDefinitions.get(name) match {
      case None => Compilation.fail(Error.TypeNotFound(name, associatedNode))
      case Some(definition) => Compilation.succeed(definition)
    }
  }

  // TODO: Also introduce getClassDefinition and getLabelDefinition. Use require/isInstanceOf and asInstanceOf.

  /**
    * Resolves a declared type with the given name.
    *
    * @param associatedNode The node where the type name occurs, to be used for error building.
    */
  def resolveType(name: String, associatedNode: Node): C[Type] = {
    // TODO: We should rename this to getType or something similar.
    types.get(name) match {
      case None => Compilation.fail(Error.TypeNotFound(name, associatedNode))
      case Some(tpe) => Compilation.succeed(tpe)
    }
  }

  /**
    * Resolves the declared supertype with the given name.
    */
  def resolveSupertype(maybeName: Option[String], associatedNode: TypeExprNode): C[Option[Type]] = {
    maybeName.map(name => resolveType(name, associatedNode)).toCompiledOption
  }

  /* def addFunction(function: LoreFunction): Unit = {
    val multiFunction = multiFunctions.getOrElse(function.name, MultiFunction(function.name, Set()))
    multiFunctions.put(function.name, MultiFunction(function.name, Set(function) ++ multiFunction.functions))
  } */

}
