package lore.compiler.feedback

import lore.compiler.core.Position
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.structures.{StructBinding, StructObjectBinding, StructPropertyDefinition}
import lore.compiler.syntax.DeclNode.PropertyNode
import lore.compiler.types.{ShapeType, StructSchema, TypeVariable}

object StructFeedback {

  case class DuplicateProperty(schema: StructSchema, property: StructPropertyDefinition) extends Feedback.Error(property) {
    override def message = s"The property `${property.name}` is declared twice in the struct `${schema.name}`."
  }

  case class MutableOpenProperty(node: PropertyNode) extends Feedback.Error(node) {
    override def message = s"The open property `${node.name}` may not be mutable."
  }

  case class ConstructorExpected(name: NamePath, override val position: Position) extends Feedback.Error(position) {
    override def message: String = s"The type `$name` doesn't have an associated constructor, as it isn't a struct."
  }

  case class CompanionModuleExpected(
    binding: StructBinding,
    memberName: String,
    override val position: Position,
  ) extends Feedback.Error(position) {
    override def message: String = s"The struct `${binding.name}` does not have a companion module which" +
      s" might define a member `$memberName`."
  }

  object Instantiation {
    case class DuplicateProperty(name: String, override val position: Position) extends Feedback.Error(position) {
      override def message: String = s"The property `$name` occurs more than once in the instantiation. Properties must" +
        s" be unique here."
    }

    case class MissingProperty(name: String, override val position: Position) extends Feedback.Error(position) {
      override def message: String = s"The struct's property `$name` must be specified in the instantiation."
    }

    case class IllegalProperty(name: String, override val position: Position) extends Feedback.Error(position) {
      override def message: String = s"The struct to be instantiated does not have a property `$name`."
    }
  }

  object Shape {
    case class MissingProperty(schema: StructSchema, property: ShapeType.Property) extends Feedback.Error(schema) {
      override def message: String = s"The struct `${schema.name}` should declare a property '${property.name}' of" +
        s" type `${property.tpe}` (or a subtype thereof) according to the struct's inherited shape type. Such a" +
        s" property is missing."
    }

    case class InvalidPropertyType(
      schema: StructSchema,
      structProperty: StructPropertyDefinition,
      shapeProperty: ShapeType.Property,
    ) extends Feedback.Error(structProperty) {
      override def message: String = s"The property `${structProperty.name}` should have the type `${shapeProperty.tpe}`" +
        s" (or a subtype thereof), but actually has the type `${structProperty.tpe}`."
    }
  }

  object OpenTypeParameter {
    case class CovarianceRequired(
      typeParameter: TypeVariable,
      override val position: Position,
    ) extends Feedback.Error(position) {
      override def message: String = s"The open type parameter `$typeParameter` must be covariant."
    }

    case class IllegalLowerBound(
      typeParameter: TypeVariable,
      override val position: Position,
    ) extends Feedback.Error(position) {
      override def message: String = s"The open type parameter `$typeParameter` may not have a lower bound."
    }

    case class NotUniquelyDeducible(
      typeParameter: TypeVariable,
      override val position: Position,
    ) extends Feedback.Error(position) {
      override def message: String = s"The open type parameter `$typeParameter` is not uniquely deducible. It may only" +
        s" be used once in a single property, and not within a sum or intersection type."
    }

    case class MutableProperty(
      typeParameter: TypeVariable,
      property: StructPropertyDefinition,
    ) extends Feedback.Error(property) {
      override def message: String = s"The open type parameter `$typeParameter` is used in a mutable property `$property`." +
        s" It may only be used in an immutable property."
    }
  }

  object Object {
    case class MissingDefault(schema: StructSchema, property: StructPropertyDefinition) extends Feedback.Error(property) {
      override def message: String = s"The property `${property.name}` must have a default value because `${schema.name}`" +
        s" is an object. Objects cannot be instantiated directly, so the default value ensures that each property is" +
        s" assigned an appropriate value."
    }

    case class NoConstructor(name: NamePath, override val position: Position) extends Feedback.Error(position) {
      override def message: String = s"The type `$name` is an object, which doesn't have a constructor. Objects cannot be" +
        s" constructed. You can refer to the object value simply by `$name`."
    }

    case class MemberNameTaken(
      schema: StructSchema,
      name: String,
      memberPosition: Position,
    ) extends Feedback.Error(memberPosition) {
      override def message: String = s"The struct object `${schema.name.simpleName}` already has a property `$name`." +
        s" Companion module members and struct properties may not share names."
    }

    case class MemberNotFound(
      binding: StructObjectBinding,
      memberName: String,
      override val position: Position,
    ) extends Feedback.Error(position) {
      override def message: String = s"The struct object `${binding.name}` does not have a property `$memberName`," +
        s" nor a companion module which might define such a member."
    }
  }

}
