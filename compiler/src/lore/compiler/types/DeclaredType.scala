package lore.compiler.types

import lore.compiler.semantics.structures.DeclaredTypeDefinition
import lore.compiler.utils.CollectionExtensions.FilterTypeExtension

trait DeclaredType extends NamedType {
  /**
    * The name of the declared type.
    */
  def name: String

  /**
    * The supertypes of the declared type. Only traits and component types are currently allowed by the grammar to
    * be supertypes of a declared type.
    */
  def supertypes: Vector[Type]

  /**
    * The component types that this entity inherits from.
    */
  def componentTypes: Vector[ComponentType] = supertypes.filte

  /**
    * Whether the declared type is an entity, i.e. it contains one or more components.
    */
  def isEntity: Boolean = supertypes.filterType[ComponentType].nonEmpty || supertypes.filterType[DeclaredType].exists(_.isEntity)

  /**
    * The supertypes of the declared type that directly inherit from Any, possibly this type itself.
    */
  def rootSupertypes: Vector[Type] = if (supertypes.isEmpty) Vector(this) else supertypes.flatMap {
    case dt: DeclaredType => dt.rootSupertypes
    case t => Vector(t)
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
