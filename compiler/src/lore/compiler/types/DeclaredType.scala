package lore.compiler.types

import lore.compiler.semantics.structures.DeclaredTypeDefinition
import lore.compiler.utils.CollectionExtensions._

trait DeclaredType extends NamedType {

  /**
    * The name of the declared type.
    */
  def name: String

  /**
    * The direct supertypes of the declared type. Only traits and component types are currently allowed by the grammar
    * to be supertypes of a declared type.
    */
  def supertypes: Vector[Type]

  // TODO: A supertype list that contains ALL supertypes transitively without removing subsumed types, which could
  //       then be used to very quickly look up whether this type is a subtype of a given type without having to
  //       run up the supertype tree. This optimization is especially important for the runtime.

  /**
    * All direct declared supertypes of the declared type.
    */
  lazy val declaredSupertypes: Vector[DeclaredType] = supertypes.filterType[DeclaredType]

  /**
    * All direct component supertypes of the declared type.
    */
  lazy val componentSupertypes: Vector[ComponentType] = supertypes.filterType[ComponentType]

  /**
    * The component types that this declared type directly and indirectly inherits. This is an exhaustive list
    * of all component types across the supertype hierarchy of this declared type. Since specialized component
    * types subsume more general component types, the latter is also removed from the list.
    *
    * For example, take code such as this:
    *   trait AnimalHousing extends +Animal, +Roof, +Walls
    *   struct DogHouse implements AnimalHousing { component Dog, component SturdyRoof, component BrickWalls }
    *
    * The component types (as defined by this function) of DogHouse are: +Dog, +Roof, +Walls. Notably, +Animal
    * has been removed from the list as it is subsumed by +Dog.
    */
  lazy val inheritedComponentTypes: Set[ComponentType] = {
    val all = componentSupertypes ++ declaredSupertypes.flatMap(_.inheritedComponentTypes)
    Type.mostSpecific(all.toSet).asInstanceOf[Set[ComponentType]]
  }

  /**
    * Whether the declared type is an entity, i.e. it contains one or more components.
    */
  lazy val isEntity: Boolean = inheritedComponentTypes.nonEmpty

  /**
    * The type that an entity owning this component must subtype. This type may be Any, which means that the
    * declared type may be owned by any kind of entity.
    *
    * ownedBy is declared in DeclaredTypeDefinition, because once definitions are resolved, all types will have been
    * loaded and the ownedBy type doesn't need to be resolved with a deferred approach.
    */
  def ownedBy: Type = definition.ownedBy

  /**
    * The supertypes of the declared type that directly inherit from Any, possibly this type itself and
    * including component types.
    */
  def rootSupertypes: Vector[Type] = {
    if (supertypes.isEmpty) {
      Vector(this)
    } else {
      supertypes.flatMap {
        case dt: DeclaredType => dt.rootSupertypes
        case t => Vector(t)
      }
    }
  }

  /**
    * The definition associated with this type.
    */
  def definition: DeclaredTypeDefinition

  // We define equality of declared types as nominal equality.
  override def equals(obj: Any): Boolean = obj match {
    case rhs: DeclaredType => this.eq(rhs) || name == rhs.name
    case _ => false
  }
  override lazy val hashCode: Int = name.hashCode

}

object DeclaredType {
  trait DefinitionProperty[Def <: DeclaredTypeDefinition] { self: DeclaredType =>
    private var _def: Def = _
    override def definition: Def = _def

    /**
      * Initializes the declared type with its associated declared type definition.
      */
    def initialize(definition: Def): Unit = {
      assert(this._def == null)
      this._def = definition
    }
  }
}
