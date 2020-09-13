package lore.compiler.semantics.structures

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, CompilationException, Error, Position}
import lore.compiler.semantics.functions.{ConstructorDefinition, FunctionSignature}
import lore.compiler.semantics.structures.ClassDefinition.ConstructorNotFound
import lore.compiler.types.{StructType, ComponentType, Type}
import lore.compiler.utils.CollectionExtensions._

import scala.collection.mutable

/**
  * The definition of both a class and an entity.
  *
  * @param localMembers The members declared within this class/entity. Does not include supertype properties.
  */
class ClassDefinition(
  override val name: String,
  override val tpe: StructType,
  val ownedBy: Option[Type],
  val isEntity: Boolean,
  val localMembers: List[MemberDefinition],
  override val position: Position,
) extends DeclaredTypeDefinition {
  // Many of the members here are declared as vals. This is only possible because definitions are created according
  // to the inheritance hierarchy.

  override val supertypeDefinition: Option[ClassDefinition] = tpe.supertypes.map(_.definition)

  /**
    * The list of all properties belonging to this class, excluding superclass components.
    */
  val localProperties: List[PropertyDefinition] = localMembers.filterType[PropertyDefinition]

  /**
    * The list of all components belonging to this class, excluding superclass components.
    */
  val localComponents: List[ComponentDefinition] = localMembers.filterType[ComponentDefinition]

  /**
    * A map of overridden names pointing to the names of their overriding components.
    */
  val overriddenToOverrider: Map[String, String] = localComponents.flatMap(m => m.overrides.map(_ -> m.name)).toMap

  /**
    * The list of all members belonging to this class, including superclass members. This list EXCLUDES overridden
    * components defined in superclasses.
    */
  val members: List[MemberDefinition] = {
    val all = supertypeDefinition.map(_.members).getOrElse(List.empty) ++ localMembers
    // Exclude overridden components.
    all.filterNot(m => overriddenToOverrider.contains(m.name))
  }

  /**
    * The list of all properties belonging to this class, including superclass properties.
    */
  val properties: List[PropertyDefinition] = members.filterType[PropertyDefinition]

  /**
    * The list of all components belonging to this class, including superclass components.
    */
  val components: List[ComponentDefinition] = members.filterType[ComponentDefinition]

  /**
    * The signature of the local construct "function". This does not include any arguments to be passed to the
    * super type.
    */
  lazy val constructSignature: FunctionSignature = FunctionSignature("construct", localMembers.map(_.asParameter), tpe, position)

  /**
    * Whether the class definition can still be altered by mutations such as registering constructors.
    */
  private var _isFinalized = false

  /**
    * Finalize the definition, disallowing any mutations from here on.
    */
  def finalizeDefinition(): Unit = {
    if (_isFinalized) throw CompilationException(s"Cannot finalize class definition $name twice.")
    _isFinalized = true
  }

  /**
    * A map of registered constructors.
    */
  private val _constructors: mutable.HashMap[String, ConstructorDefinition] = mutable.HashMap()

  /**
    * All constructors registered in this class.
    */
  lazy val constructors: List[ConstructorDefinition] = {
    if (!_isFinalized) {
      throw CompilationException("ClassDefinition.constructors can't be used until the definition has been finalized.")
    }
    _constructors.values.toList
  }

  /**
    * Registers a constructor.
    */
  def registerConstructor(constructor: ConstructorDefinition): Unit = {
    if (_isFinalized) {
      throw CompilationException(s"Constructors can't be registered with a finalized class definition $name.")
    }
    if (_constructors.contains(constructor.name)) {
      throw CompilationException(s"The constructor ${constructor.name} has already been registered in class $name.")
    }

    constructor.associateWith(this)
    _constructors.put(constructor.name, constructor)
  }

  /**
    * Searches for a constructor with the given name.
    */
  def getConstructor(constructorName: String): Option[ConstructorDefinition] = _constructors.get(constructorName)

  /**
    * Resolves a constructor of the given class.
    */
  def resolveConstructor(constructorName: String)(implicit position: Position): Compilation[ConstructorDefinition] = {
    getConstructor(constructorName) match {
      case None => Compilation.fail(ConstructorNotFound(name, constructorName))
      case Some(constructor) => constructor.compiled
    }
  }

  /**
    * The class's default constructor. If this constructor is not registered, we throw a compilation exception.
    */
  lazy val defaultConstructor: ConstructorDefinition = getConstructor(name) match {
    case None => throw CompilationException(s"The class $name has no default constructor, but one should have been generated by the compiler!")
    case Some(definition) => definition
  }

  /**
    * Returns all component types that this class has in common with the other given class.
    *
    * If two components match, but one is a supertype of the other, the supertype will be chosen.
    */
  def commonComponentTypes(other: ClassDefinition): List[ComponentType] = {
    components.map(_.tpe).flatMap { left =>
      val commonTypes = other.components.map(_.tpe).flatMap { right =>
        if (left <= right) Some(right)
        else if (left >= right) Some(left)
        else None
      }

      // There should only be exactly one type that the other entity has in common with this entity. If not, one of
      // the entities is violating the component subtyping hierarchy constraint.
      assert(commonTypes.size <= 1)

      commonTypes.map(ComponentType)
    }
  }
}

object ClassDefinition {
  case class ConstructorNotFound(className: String, name: String)(implicit position: Position) extends Error(position) {
    override def message = s"The constructor $className.$name does not exist."
  }
}
