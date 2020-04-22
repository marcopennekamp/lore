package lore.types

import lore.compiler.Registry
import lore.definitions.DeclaredTypeDefinition

/**
  * A declared type as defined by the spec.
  */
trait DeclaredType extends Type {
  /**
    * The definition associated with this type.
    */
  def definition: DeclaredTypeDefinition

  /**
    * The supertype of the declared type.
    */
  def supertype: Option[DeclaredType]

  /**
    * Returns the set of explicitly declared immediate subtypes, for example direct subclasses or direct
    * sub-label types.
    */
  def directDeclaredSubtypes(implicit registry: Registry): Set[Type] = {
    // We need the .toSet at the end to cast DeclaredType to Type, since sets are invariant.
    registry.declaredTypeHierarchy.getDirectSubtypes(this).toSet
  }

  /**
    * A verbose string representation of the type.
    */
  def verbose: String = toString

  override def string(precedence: TypePrecedence): String = definition.name
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
