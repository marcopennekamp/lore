package lore.compiler.feedback

import lore.compiler.core.Position
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.scopes.{StructBinding, StructObjectBinding}
import lore.compiler.semantics.structures.{StructDefinition, StructPropertyDefinition}
import lore.compiler.types.{ShapeType, TypeVariable}

object StructFeedback {

  case class DuplicateProperty(definition: StructDefinition, property: StructPropertyDefinition) extends Feedback.Error(property) {
    override def message = s"The property ${property.name} is declared twice in the struct ${definition.name}."
  }

  case class ConstructorExpected(name: NamePath, override val position: Position) extends Feedback.Error(position) {
    override def message: String = s"The type $name doesn't have an associated constructor. It must be a struct."
  }

  case class CompanionModuleExpected(binding: StructBinding, memberName: String, override val position: Position) extends Feedback.Error(position) {
    override def message: String = s"The struct ${binding.definition.name} does not have a companion module which" +
      s" might define a member $memberName."
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

  object Object {
    case class MissingDefault(struct: StructDefinition, property: StructPropertyDefinition) extends Feedback.Error(property) {
      override def message: String = s"The property ${property.name} must have a default value because ${struct.name}" +
        s" is an object. Objects cannot be instantiated directly, so the default value ensures that each property is" +
        s" assigned an appropriate value."
    }

    case class NoConstructor(name: NamePath, override val position: Position) extends Feedback.Error(position) {
      override def message: String = s"The type $name is an object, which doesn't have a constructor. Objects cannot be" +
        s" constructed. You can refer to the object value simply by the variable $name."
    }

    case class MemberNameTaken(struct: StructDefinition, name: String, memberPosition: Position) extends Feedback.Error(memberPosition) {
      override def message: String = s"The struct object ${struct.name.simpleName} already has a property $name." +
        s" Companion module members and struct properties may not share names."
    }

    case class CompanionModuleExpected(binding: StructObjectBinding, memberName: String, override val position: Position) extends Feedback.Error(position) {
      override def message: String = s"The struct object ${binding.definition.name} does not have a property $memberName," +
        s" nor a companion module which might define such a member."
    }
  }

}
