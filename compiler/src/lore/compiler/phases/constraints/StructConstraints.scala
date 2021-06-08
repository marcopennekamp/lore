package lore.compiler.phases.constraints

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.Compilation
import lore.compiler.feedback.Feedback
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.{StructDefinition, StructPropertyDefinition}
import lore.compiler.types.ShapeType
import lore.compiler.utils.CollectionExtensions.VectorExtension

object StructConstraints {

  /**
    * Verifies:
    *   1. Properties must be unique.
    *   2. The properties of the struct's inherited shape type must all be implemented.
    */
  def verify(definition: StructDefinition)(implicit registry: Registry): Verification = {
    (
      verifyPropertiesUnique(definition),
      verifyInheritedShapeProperties(definition),
    ).simultaneous.verification
  }

  case class DuplicateProperty(definition: StructDefinition, property: StructPropertyDefinition) extends Feedback.Error(property) {
    override def message = s"The property ${property.name} is declared twice in the struct ${definition.name}."
  }

  /**
    * Verifies that this struct's properties are unique.
    */
  private def verifyPropertiesUnique(definition: StructDefinition): Verification = {
    definition.properties.requireUnique(_.name, property => DuplicateProperty(definition, property)).verification
  }

  case class ShapeMissingProperty(definition: StructDefinition, property: ShapeType.Property) extends Feedback.Error(definition) {
    override def message: String = s"The struct ${definition.name} should declare a property '${property.name}' of type " +
      s"${property.tpe} (or a subtype thereof) according to the struct's inherited shape type. Such a property is missing."
  }

  case class ShapeInvalidPropertyType(
    definition: StructDefinition,
    structProperty: StructPropertyDefinition,
    shapeProperty: ShapeType.Property,
  ) extends Feedback.Error(structProperty) {
    override def message: String = s"The property '${structProperty.name}' should have the type ${shapeProperty.tpe} " +
      s"(or a subtype thereof), but actually has the type ${structProperty.tpe}."
  }

  /**
    * Verifies that the struct's inherited shape type is properly implemented.
    */
  private def verifyInheritedShapeProperties(definition: StructDefinition): Verification = {
    definition.tpe.inheritedShapeType.properties.values.toVector.map { shapeProperty =>
      definition.propertyMap.get(shapeProperty.name) match {
        case Some(structProperty) =>
          if (structProperty.tpe <= shapeProperty.tpe) Verification.succeed
          else Compilation.fail(ShapeInvalidPropertyType(definition, structProperty, shapeProperty))
        case None => Compilation.fail(ShapeMissingProperty(definition, shapeProperty))
      }
    }.simultaneous.verification
  }

}
