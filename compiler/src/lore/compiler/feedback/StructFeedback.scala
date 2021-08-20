package lore.compiler.feedback

import lore.compiler.core.Position
import lore.compiler.semantics.structures.{StructDefinition, StructPropertyDefinition}
import lore.compiler.types.{ShapeType, TypeVariable}

object StructFeedback {

  case class DuplicateProperty(definition: StructDefinition, property: StructPropertyDefinition) extends Feedback.Error(property) {
    override def message = s"The property ${property.name} is declared twice in the struct ${definition.name}."
  }

  object Shape {
    case class MissingProperty(definition: StructDefinition, property: ShapeType.Property) extends Feedback.Error(definition) {
      override def message: String = s"The struct ${definition.name} should declare a property '${property.name}' of type " +
        s"${property.tpe} (or a subtype thereof) according to the struct's inherited shape type. Such a property is missing."
    }

    case class InvalidPropertyType(
      definition: StructDefinition,
      structProperty: StructPropertyDefinition,
      shapeProperty: ShapeType.Property,
    ) extends Feedback.Error(structProperty) {
      override def message: String = s"The property '${structProperty.name}' should have the type ${shapeProperty.tpe} " +
        s"(or a subtype thereof), but actually has the type ${structProperty.tpe}."
    }
  }

  object OpenTypeParameter {
    case class CovarianceRequired(typeParameter: TypeVariable, override val position: Position) extends Feedback.Error(position) {
      override def message: String = s"The open type parameter $typeParameter must be covariant."
    }

    case class NotUniquelyDeducible(typeParameter: TypeVariable, override val position: Position) extends Feedback.Error(position) {
      override def message: String = s"The open type parameter $typeParameter is not uniquely deducible. It may only be" +
        s" used once in a single property, and not within a sum or intersection type."
    }

    case class MutableProperty(typeParameter: TypeVariable, property: StructPropertyDefinition) extends Feedback.Error(property) {
      override def message: String = s"The open type parameter $typeParameter is used in a mutable property $property." +
        s" It may only be used in an immutable property."
    }
  }

}
