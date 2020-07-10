package lore.compiler.types

import lore.compiler.core.Registry
import lore.compiler.structures.DeclaredTypeDefinition

/**
  * A declared type as defined by the spec.
  */
trait DeclaredType extends NamedType {
  /**
    * The definition associated with this type.
    */
  def definition: DeclaredTypeDefinition

  /**
    * The name of the declared type.
    */
  def name: String = definition.name

  /**
    * The supertype of the declared type.
    */
  def supertype: Option[DeclaredType]

  /**
    * The supertype of the declared type that directly inherits from Any, possibly this type itself.
    */
  def rootSupertype: DeclaredType = supertype match {
    case None => this
    case Some(tpe) => tpe.rootSupertype
  }

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

  override def string(precedence: TypePrecedence): String = name

  // TODO: For now. This needs to be set to true for classes with type parameters, of course.
  override def isPolymorphic = false

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
