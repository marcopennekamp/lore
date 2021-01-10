package lore.compiler.types

import lore.compiler.semantics.structures.{StructDefinition, StructPropertyDefinition}

/**
  * @param supertypes The list of supertypes of the struct, which include all traits the struct implements.
  */
class StructType(
  override val name: String,
  override val supertypes: Vector[Type],
) extends DeclaredType with DeclaredType.DefinitionProperty[StructDefinition] {

  /**
    * The struct viewed as a compile-time shape type. Whether the struct's properties are open has no bearing on
    * this representation.
    */
  override lazy val asShapeType: ShapeType = ShapeType(definition.properties.map(ShapeType.Property.apply))

  val hasOpenProperties = false
  val openProperties: Vector[StructPropertyDefinition] = Vector.empty

}
