package lore.compiler.types

import lore.compiler.core.Registry
import lore.compiler.structures.DeclaredTypeDefinition
import lore.types.Type

/**
  * A declared type as defined by the spec.
  */
trait DeclaredType extends lore.types.DeclaredType {
  /**
    * The definition associated with this type.
    */
  def definition: DeclaredTypeDefinition
  override def name: String = definition.name

  // We need to override these because we have to assert to Scala that in the context of the compiler,
  // we are always expecting lore.compiler types.
  override def supertype: Option[DeclaredType]
  override def rootSupertype: DeclaredType = super.rootSupertype.asInstanceOf[DeclaredType]

  /**
    * Returns the set of explicitly declared immediate subtypes, for example direct subclasses or direct
    * sub-label types.
    */
  def directDeclaredSubtypes(implicit registry: Registry): Set[Type] = {
    // We need the .toSet at the end to cast DeclaredType to Type, since sets are invariant.
    registry.declaredTypeHierarchy.getDirectSubtypes(this).toSet
  }
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
