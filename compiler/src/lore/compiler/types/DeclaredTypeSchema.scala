package lore.compiler.types

import lore.compiler.core.Registry
import lore.compiler.definitions.DeclaredTypeDefinition
import lore.types
import lore.types.Type

trait DeclaredTypeSchema extends lore.types.DeclaredTypeSchema {
  /**
    * The definition associated with this type schema.
    */
  def definition: DeclaredTypeDefinition
  override def name: String = definition.name

  // We need to override these because we have to assert to Scala that in the context of the compiler,
  // we are always expecting lore.compiler schemas.
  override def superschema: Option[DeclaredTypeSchema]
  override def rootSuperschema: DeclaredTypeSchema = super.rootSuperschema.asInstanceOf[DeclaredTypeSchema]

  /**
    * Returns the set of explicitly declared immediate subtypes, for example direct subclasses or direct
    * sub-label types.
    */
  def directDeclaredSubtypes(implicit registry: Registry): Set[Type] = {
    // We need the .toSet at the end to cast DeclaredType to Type, since sets are invariant.
    registry.declaredTypeHierarchy.getDirectSubtypes(this).toSet
  }
}

object DeclaredTypeSchema {
  trait DefinitionProperty[Def <: DeclaredTypeDefinition] { self: DeclaredTypeSchema =>
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

  object Any extends DeclaredTypeSchema {
    override def definition: DeclaredTypeDefinition = null
    override def superschema: Option[DeclaredTypeSchema] = ???
    /**
      * Whether the type instantiated by this schema is abstract.
      */
    override def isAbstract: Boolean = ???
    /**
      * All type parameters of the type schema in order.
      */
    override def typeParameters: List[types.TypeVariable] = ???
    /**
      * Instantiates the type schema with the given type arguments. If you are unsure whether this operation
      * will go smoothly, call canInstantiate.
      */
    override def instantiate(types: List[Type]): types.DeclaredType = ???
  }
}
