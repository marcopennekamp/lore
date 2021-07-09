package lore.compiler.phases.resolution

import lore.compiler.feedback.FeedbackExtensions.FilterDuplicatesExtension
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.syntax.TypeExprNode
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.{OptionTuple2Extension, OptionVectorExtension}

object TypeExpressionEvaluator {

  case class DuplicateProperty(property: TypeExprNode.ShapePropertyNode) extends Feedback.Error(property) {
    override def message = s"The property ${property.name} is declared twice in the shape type. Shape type properties must be unique."
  }

  def evaluate(expression: TypeExprNode)(implicit typeScope: TypeScope, reporter: Reporter): Option[Type] = {
    expression match {
      case TypeExprNode.IdentifierNode(name, position) => typeScope.resolve(name, position)
      case TypeExprNode.IntersectionNode(expressions, _) => expressions.map(evaluate).sequence.map(IntersectionType.construct)
      case TypeExprNode.SumNode(expressions, _) => expressions.map(evaluate).sequence.map(SumType.construct)
      case TypeExprNode.TupleNode(expressions, _) => expressions.map(evaluate).sequence.map(TupleType(_))
      case TypeExprNode.UnitNode(_) => Some(TupleType.UnitType)
      case TypeExprNode.FunctionNode(input, output, _) => (evaluate(input).map(Type.tupled), evaluate(output)).sequence.map(FunctionType.tupled)
      case TypeExprNode.ListNode(element, _) => evaluate(element).map(ListType)
      case TypeExprNode.MapNode(key, value, _) => (evaluate(key), evaluate(value)).sequence.map(MapType.tupled)
      case node@TypeExprNode.ShapeNode(_, _) => Some(evaluateShape(node))
      case TypeExprNode.SymbolNode(name, _) => Some(SymbolType(name))
    }
  }

  private def evaluateShape(expression: TypeExprNode.ShapeNode)(implicit typeScope: TypeScope, reporter: Reporter): ShapeType = {
    ShapeType(
      expression.properties
        .filterDuplicates(_.name, DuplicateProperty)
        .map(evaluateShapeProperty)
    )
  }

  private def evaluateShapeProperty(expression: TypeExprNode.ShapePropertyNode)(implicit typeScope: TypeScope, reporter: Reporter): ShapeType.Property = {
    val tpe = evaluate(expression.tpe).getOrElse(BasicType.Any)
    ShapeType.Property(expression.name, tpe)
  }

}
