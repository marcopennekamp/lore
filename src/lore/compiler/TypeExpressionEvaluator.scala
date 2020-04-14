package lore.compiler

import lore.ast.TypeExprNode
import lore.types.{ClassType, ComponentType, IntersectionType, ListType, MapType, ProductType, SumType, Type}

object TypeExpressionEvaluator {
  def evaluate(expression: TypeExprNode)(implicit registry: Registry): C[Type] = {
    val eval = evaluate _
    expression match {
      case TypeExprNode.NominalNode(name) => registry.resolveType(name, expression)
      case TypeExprNode.IntersectionNode(expressions) => expressions.toList.map(eval).combine.map(IntersectionType.construct)
      case TypeExprNode.SumNode(expressions) => expressions.toList.map(eval).combine.map(SumType.construct)
      case TypeExprNode.ProductNode(expressions) => expressions.map(eval).combine.map(ProductType(_))
      case TypeExprNode.UnitNode => Compilation.succeed(ProductType.UnitType)
      case TypeExprNode.ListNode(element) => eval(element).map(ListType)
      case TypeExprNode.MapNode(key, value) =>
        // Use simultaneous compilation to aggregate errors from both the key and value side. If we used flatMap here,
        // we couldn't report errors about both key and value at the same time (during the same compiler run).
        (eval(key), eval(value)).combine.map(MapType.tupled)
      case componentNode@TypeExprNode.ComponentNode(underlying) => registry.resolveType(underlying, componentNode).flatMap {
        case tpe: ClassType => Compilation.succeed(ComponentType(tpe))
        case _ => Compilation.fail(Feedback.ComponentTypeMustContainClass(componentNode))
      }
    }
  }
}
