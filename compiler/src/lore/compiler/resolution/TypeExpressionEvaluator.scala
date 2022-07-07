package lore.compiler.resolution

import lore.compiler.feedback.FeedbackExtensions.FilterDuplicatesExtension
import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.semantics.scopes.{TermScope, TypeScope}
import lore.compiler.syntax.TypeExprNode
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.{OptionTuple2Extension, OptionVectorExtension}

object TypeExpressionEvaluator {

  /**
    * @param termScope The term scope is required to properly resolve modules containing types, for qualified type
    *                  names.
    */
  def evaluate(expression: TypeExprNode)(
    implicit typeScope: TypeScope,
    termScope: TermScope,
    reporter: Reporter,
  ): Option[Type] = {
    expression match {
      case node@TypeExprNode.TypeNameNode(_, position) => typeScope.resolveStatic(node.namePath, position).map {
        case tpe: NamedType => tpe
        case schema: NamedSchema =>
          if (!schema.isConstant) {
            reporter.error(TypingFeedback.Schema.IllegalArity(schema, 0, expression.position))
          }
          // Even if the schema isn't constant, the schema will still be instantiated with best-guess type arguments.
          schema.instantiate(Vector.empty, expression.position)
      }

      case TypeExprNode.InstantiationNode(nameNode, argumentNodes, _) => typeScope.resolveStatic(nameNode.namePath, nameNode.position).map {
        case tpe: NamedType =>
          reporter.error(TypingFeedback.Schema.ConstantUseRequired(tpe, expression))
          tpe
        case schema: NamedSchema =>
          schema.instantiate(argumentNodes.map(evaluate), expression.position)
      }

      case TypeExprNode.SymbolNode(name, _) => Some(SymbolType(name))
      case TypeExprNode.SumNode(expressions, _) => expressions.map(evaluate).sequence.map(SumType.construct)
      case TypeExprNode.IntersectionNode(expressions, _) => expressions.map(evaluate).sequence.map(IntersectionType.construct)
      case TypeExprNode.TupleNode(expressions, _) => expressions.map(evaluate).sequence.map(TupleType(_))
      case TypeExprNode.UnitNode(_) => Some(TupleType.UnitType)
      case TypeExprNode.FunctionNode(input, output, _) => (evaluate(input).map(Type.tupled), evaluate(output)).sequence.map(FunctionType.tupled)
      case TypeExprNode.ListNode(element, _) => evaluate(element).map(ListType)
      case TypeExprNode.MapNode(key, value, _) => (evaluate(key), evaluate(value)).sequence.map(MapType.tupled)
      case node@TypeExprNode.ShapeNode(_, _) => Some(evaluateShape(node))
    }
  }

  private def evaluateShape(expression: TypeExprNode.ShapeNode)(
    implicit typeScope: TypeScope,
    termScope: TermScope,
    reporter: Reporter,
  ): ShapeType = {
    ShapeType(
      expression.properties
        .filterDuplicates(_.name, TypingFeedback.Shape.DuplicateProperty)
        .map(evaluateShapeProperty)
    )
  }

  private def evaluateShapeProperty(expression: TypeExprNode.ShapePropertyNode)(
    implicit typeScope: TypeScope,
    termScope: TermScope,
    reporter: Reporter,
  ): ShapeType.Property = {
    val tpe = evaluate(expression.tpe).getOrElse(BasicType.Any)
    ShapeType.Property(expression.name, tpe)
  }

}
