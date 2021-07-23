package lore.compiler.types

import lore.compiler.semantics.structures.StructPropertyDefinition


case class StructType(
  schema: StructSchema,
  typeArguments: TypeVariable.Assignments,
) extends DeclaredType {

  /**
    * The struct's properties, their types instantiated with the type arguments.
    */
  lazy val properties: Vector[StructPropertyDefinition.Instance] = schema.definition.properties.map(_.instantiate(typeArguments))

  /**
    * The struct viewed as a compile-time shape type. Whether the struct's properties are open has no bearing on
    * this representation.
    */
  override lazy val asShapeType: ShapeType = ShapeType(properties.map(ShapeType.Property.apply))

}
