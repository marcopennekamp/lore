package lore.compiler.types

import lore.compiler.semantics.structures.DeclaredTypeDefinition
import lore.compiler.utils.CollectionExtensions._

trait DeclaredType extends NamedType {

  /**
    * The name of the declared type.
    */
  def name: String

  /**
    * The direct supertypes of the declared type. Only trait types are currently allowed by the grammar to be supertypes
    * of a declared type.
    *
    * TODO (shape): Shape types will be added soon, so don't rip out this abstraction.
    */
  def supertypes: Vector[Type]

  // TODO: A supertype list that contains ALL supertypes transitively without removing subsumed types, which could
  //       then be used to very quickly look up whether this type is a subtype of a given type without having to
  //       run up the supertype tree. This optimization is especially important for the runtime.

  /**
    * All direct declared supertypes of the declared type.
    */
  lazy val declaredSupertypes: Vector[DeclaredType] = supertypes.filterType[DeclaredType]

  // TODO (shape): We will likely have to write a similar definition to the one below for shape types.
  //               In concrete terms, we could create a `lazy val inheritedShapeType: ShapeType` for any given
  //               declared type that just combines all the shape types the declared type directly or indirectly
  //               extends.

  /**
    * The component types that this declared type directly and indirectly inherits. This is an exhaustive list
    * of all component types across the supertype hierarchy of this declared type. Since specialized component
    * types subsume more general component types, the latter are also removed from the list. Subsumption is decided
    * without taking owned-by types into account.
    *
    * For example, take code such as this:
    *   trait AnimalHousing extends +Animal, +Roof, +Walls
    *   struct DogHouse implements AnimalHousing { component Dog, component SturdyRoof, component BrickWalls }
    *
    * The component types (as defined by this function) of DogHouse are: +Dog, +Roof, +Walls. Notably, +Animal
    * has been removed from the list as it is subsumed by +Dog.
    */
  /* lazy val inheritedComponentTypes: Set[ComponentType] = {
    val all = componentSupertypes ++ declaredSupertypes.flatMap(_.inheritedComponentTypes)
    Type.mostSpecific(all.toSet, Subtyping.NoOwnedBy).asInstanceOf[Set[ComponentType]]
  } */

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
