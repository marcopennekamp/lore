package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, Error, Position}
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.syntax.TypeExprNode
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.VectorExtension

object TypeExpressionEvaluator {

  case class DuplicateProperty(property: TypeExprNode.ShapePropertyNode) extends Error(property) {
    override def message = s"The property ${property.name} is declared twice in the shape type. Shape type properties must be unique."
  }

  def evaluate(expression: TypeExprNode)(implicit typeScope: TypeScope): Compilation[Type] = {
    implicit val position: Position = expression.position
    expression match {
      case TypeExprNode.IdentifierNode(name, _) => typeScope.resolve(name)
      case TypeExprNode.IntersectionNode(expressions, _) => expressions.map(evaluate).simultaneous.map(IntersectionType.construct)
      case TypeExprNode.SumNode(expressions, _) => expressions.map(evaluate).simultaneous.map(SumType.construct)
      case TypeExprNode.ProductNode(expressions, _) => expressions.map(evaluate).simultaneous.map(ProductType(_))
      case TypeExprNode.UnitNode(_) => ProductType.UnitType.compiled
      case TypeExprNode.ListNode(element, _) => evaluate(element).map(ListType)
      case TypeExprNode.MapNode(key, value, _) => (evaluate(key), evaluate(value)).simultaneous.map(MapType.tupled)
      case TypeExprNode.ShapeNode(propertyNodes, _) =>
        propertyNodes
          .requireUnique(_.name, DuplicateProperty)
          .flatMap(_.map(evaluateShapeProperty).simultaneous)
          .map(ShapeType.apply)
    }
  }

  private def evaluateShapeProperty(expression: TypeExprNode.ShapePropertyNode)(implicit typeScope: TypeScope): Compilation[ShapeType.Property] = {
    evaluate(expression.tpe).map(tpe => ShapeType.Property(expression.name, tpe))
  }

}
