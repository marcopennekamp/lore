package lore.compiler.types

import lore.compiler.core.CompilationException
import lore.compiler.semantics.structures.DeclaredSchemaDefinition
import lore.compiler.utils.CollectionExtensions.VectorExtension

trait DeclaredSchema extends NamedSchema {

  override def representative: DeclaredType = super.representative.asInstanceOf[DeclaredType]

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

  /**
    * A set of <i>all</i> the schema's direct and indirect supertypes, taken from the representative and thus
    * instantiated with the type parameters of this schema where applicable. Does not contain duplicates, but may
    * contain multiple types of the same schema with different type arguments.
    */
  lazy val indirectDeclaredSupertypes: Set[DeclaredType] = representative.indirectDeclaredSupertypes

  /**
    * If a declared type inherits from the same parameterized declared type `T[A]` multiple times, but not all
    * occurrences of `T[A]` are equal, the algorithms for subtyping and type variable allocation have to fall back to a
    * more complicated version. That approach collects candidates `T[X]`, `T[Y]`, `T[Z]` across the subtyping hierarchy
    * and then combines the type arguments `X`, `Y`, and `Z` depending on whether `A` is covariant (intersection type)
    * or contravariant (sum type). `A` being invariant constitutes illegal inheritance, which is an error caught by a
    * constraint check.
    *
    * This flag allows us to use the faster algorithms at compile time <i>and</i> run time when no multiple
    * parameterized inheritance is detected.
    */
  lazy val hasMultipleParameterizedInheritance: Boolean = {
    declaredSupertypes.exists(_.schema.hasMultipleParameterizedInheritance) ||
      indirectDeclaredSupertypes.groupBy(_.schema).exists { case (_, types) => types.size > 1 }
  }

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
