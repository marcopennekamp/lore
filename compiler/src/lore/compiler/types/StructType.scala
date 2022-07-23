package lore.compiler.types

import lore.compiler.semantics.functions.FunctionSignature

case class StructType(
  schema: StructSchema,
  assignments: TypeVariable.Assignments,
) extends DeclaredType {

  /**
    * The struct's properties, their types instantiated with the type arguments.
    */
  lazy val properties: Vector[StructProperty.Instance] = schema.properties.map(_.instantiate(assignments))

  /**
    * The struct constructor signature with all type parameters instantiated. The corresponding signature type
    * parameters are thus empty.
    */
  lazy val constructorSignature: FunctionSignature = FunctionSignature(
    name,
    Vector.empty,
    properties.map(_.asParameter),
    this,
    schema.position,
  )

  /**
    * The type arguments corresponding to the schema's open type parameters.
    */
  lazy val openTypeArguments: Vector[Type] = {
    assignments.flatMap {
      case (parameter, argument) => if (parameter.isOpen) Some(argument) else None
    }.toVector
  }

  /**
    * The struct viewed as a compile-time shape type. Whether the struct's properties are open has no bearing on
    * this representation.
    */
  override lazy val asShapeType: ShapeType = ShapeType(properties.map(ShapeType.Property.apply))

}
