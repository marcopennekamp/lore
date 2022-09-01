package lore.compiler.typing2

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.typed.Expression.{ShapeProperty, ShapeValue}
import lore.compiler.semantics.expressions.untyped.UntypedExpression.UntypedShapeValue
import lore.compiler.types.ShapeType
import lore.compiler.utils.CollectionExtensions.{Tuple2OptionExtension, VectorExtension}

object ShapeTyping {

  def checkOrInfer(
    expression: UntypedShapeValue,
    expectedType: Option[ShapeType],
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    expression.properties
      .foldSome((Vector.empty[ShapeProperty], context)) {
        case ((typedProperties, context2), property) =>
          Checker2.checkOrInfer(
            property.value,
            expectedType.flatMap(_.propertyType(property.name)),
            context2,
          ).mapFirst {
            typedValue => typedProperties :+ ShapeProperty(property.name, typedValue, property.position)
          }
      }
      .mapFirst(ShapeValue(_, expression.position))
  }

}
