package lore.compiler.types

import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.semantics.structures.StructPropertyDefinition

case class StructType(
  schema: StructSchema,
  assignments: TypeVariable.Assignments,
) extends DeclaredType {

  /**
    * The struct's properties, their types instantiated with the type arguments.
    */
  lazy val properties: Vector[StructPropertyDefinition.Instance] = schema.definition.properties.map(_.instantiate(assignments))

  /**
    * The struct constructor signature with all type parameters instantiated.
    */
  lazy val constructorSignature: FunctionSignature = FunctionSignature(
    name,
    properties.map(_.tpe).flatMap(Type.variables).distinct,
    properties.map(_.asParameter),
    this,
    schema.definition.position,
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
