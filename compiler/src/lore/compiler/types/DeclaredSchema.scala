package lore.compiler.types

import lore.compiler.core.CompilationException
import lore.compiler.semantics.structures.DeclaredSchemaDefinition
import lore.compiler.utils.CollectionExtensions.VectorExtension

trait DeclaredSchema extends NamedSchema {

  /**
    * The definition associated with this schema.
    */
  def definition: DeclaredSchemaDefinition

  /**
    * The direct supertypes of the declared schema. Only traits and shapes are allowed to be supertypes of a declared
    * type. The list may contain instantiated type schemas whose type arguments contain some of the schema's type
    * variables.
    */
  def supertypes: Vector[Type]

  /**
    * All direct declared supertypes of the declared schema.
    */
  lazy val declaredSupertypes: Vector[DeclaredType] = supertypes.filterType[DeclaredType]

  /**
    * A shape type that combines all shape types that this declared schema directly or indirectly inherits from. This
    * shape type provides an exhaustive list of properties that the declared schema has to have, in one way or another.
    * Any of the schema's type parameters are contained uninstantiated in the inherited shape type.
    *
    * For structs, each property in the inherited shape type must be backed by a corresponding struct property.
    */
  lazy val inheritedShapeType: ShapeType = ShapeType.combine(supertypes.filterType[ShapeType] ++ declaredSupertypes.map(_.inheritedShapeType))

}

object DeclaredSchema {
  trait DefinitionProperty[Def <: DeclaredSchemaDefinition] extends DeclaredSchema {
    private var _def: Def = _
    override def definition: Def = _def

    /**
      * Initializes the declared schema with its associated declared schema definition.
      */
    def initialize(definition: Def): Unit = {
      if (this._def != null) {
        throw CompilationException(s"Only declared schemas without an already attached definition may be initialized." +
          s" Type name: $name. Definition name: ${definition.name}.")
      }
      this._def = definition
    }
  }
}
