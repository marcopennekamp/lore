package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, Position}
import lore.compiler.feedback.Feedback
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.syntax.TypeExprNode
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.VectorExtension

object TypeExpressionEvaluator {

  case class DuplicateProperty(property: TypeExprNode.ShapePropertyNode) extends Feedback.Error(property) {
    override def message = s"The property ${property.name} is declared twice in the shape type. Shape type properties must be unique."
  }

  def evaluate(expression: TypeExprNode)(implicit typeScope: TypeScope): Compilation[Type] = {
    expression match {
      case TypeExprNode.IdentifierNode(name, position) => typeScope.resolve(name, position)
      case TypeExprNode.IntersectionNode(expressions, _) => expressions.map(evaluate).simultaneous.map(IntersectionType.construct)
      case TypeExprNode.SumNode(expressions, _) => expressions.map(evaluate).simultaneous.map(SumType.construct)
      case TypeExprNode.TupleNode(expressions, _) => expressions.map(evaluate).simultaneous.map(TupleType(_))
      case TypeExprNode.UnitNode(_) => TupleType.UnitType.compiled
      case TypeExprNode.FunctionNode(input, output, _) => (evaluate(input).map(Type.tupled), evaluate(output)).simultaneous.map(FunctionType.tupled)
      case TypeExprNode.ListNode(element, _) => evaluate(element).map(ListType)
      case TypeExprNode.MapNode(key, value, _) => (evaluate(key), evaluate(value)).simultaneous.map(MapType.tupled)
      case TypeExprNode.ShapeNode(propertyNodes, _) =>
        propertyNodes
          .requireUnique(_.name, DuplicateProperty)
          .flatMap(_.map(evaluateShapeProperty).simultaneous)
          .map(ShapeType.apply)
      case TypeExprNode.SymbolNode(name, _) => SymbolType(name).compiled
    }
  }

  private def evaluateShapeProperty(expression: TypeExprNode.ShapePropertyNode)(implicit typeScope: TypeScope): Compilation[ShapeType.Property] = {
    evaluate(expression.tpe).map(tpe => ShapeType.Property(expression.name, tpe))
  }

}
