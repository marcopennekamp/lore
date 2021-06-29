package lore.compiler.types

import lore.compiler.core.CompilationException
import lore.compiler.semantics.structures.DeclaredTypeDefinition
import lore.compiler.utils.CollectionExtensions._

trait DeclaredType extends NamedType {

  /**
    * The direct supertypes of the declared type. Only trait and shape types are currently allowed to be supertypes
    * of a declared type.
    */
  def supertypes: Vector[Type]

  /**
    * The definition associated with this type.
    */
  def definition: DeclaredTypeDefinition

  /**
    * All direct declared supertypes of the declared type.
    */
  lazy val declaredSupertypes: Vector[DeclaredType] = supertypes.filterType[DeclaredType]

  /**
    * A shape type that combines all shape types that this declared type directly or indirectly inherits from. This
    * shape type provides an exhaustive list of properties that the declared type has to have, in one way or another.
    *
    * For structs, each property in the inherited shape type must be backed by a corresponding struct property.
    */
  lazy val inheritedShapeType: ShapeType = ShapeType.combine(supertypes.filterType[ShapeType] ++ declaredSupertypes.map(_.inheritedShapeType))

  /**
    * The declared type viewed as a compile-time shape type. By default, this is equal to the [[inheritedShapeType]].
    * Structs, however, implement `asShapeType` based on their properties.
    */
  def asShapeType: ShapeType = inheritedShapeType

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
      if (this._def != null) {
        throw CompilationException(s"Only declared types without an already attached definition may be initialized." +
          s" Type name: ${self.name}. Definition name: ${definition.name}.")
      }
      this._def = definition
    }
  }
}
