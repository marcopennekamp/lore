package lore.compiler.types

import lore.compiler.ast.TypeExprNode
import lore.compiler.core.Compilation.C
import lore.compiler.core.{Compilation, Fragment, TypeScope}
import lore.compiler.feedback.Error
import lore.compiler.types
import lore.types._

object TypeExpressionEvaluator {
  case class ComponentTypeMustContainClass(node: TypeExprNode.ComponentNode)(implicit fragment: Fragment) extends Error(node) {
    override def message: String = s"The component type +${node.underlyingName} must contain a class type. ${node.underlyingName} is not a class."
  }

  def evaluate(expression: TypeExprNode)(implicit typeScope: TypeScope, fragment: Fragment): C[Type] = {
    val eval = evaluate _
    expression match {
      case TypeExprNode.NominalNode(name) => typeScope.resolve(name, expression)
      case TypeExprNode.IntersectionNode(expressions) => expressions.map(eval).simultaneous.map(IntersectionType.construct)
      case TypeExprNode.SumNode(expressions) => expressions.map(eval).simultaneous.map(SumType.construct)
      case TypeExprNode.ProductNode(expressions) => expressions.map(eval).simultaneous.map(ProductType(_))
      case TypeExprNode.UnitNode => Compilation.succeed(ProductType.UnitType)
      case TypeExprNode.ListNode(element) => eval(element).map(ListType)
      case TypeExprNode.MapNode(key, value) =>
        // Use simultaneous compilation to aggregate errors from both the key and value side. If we used flatMap here,
        // we couldn't report errors about both key and value at the same time (during the same compiler run).
        (eval(key), eval(value)).simultaneous.map(MapType.tupled)
      case componentNode@TypeExprNode.ComponentNode(underlying) => typeScope.resolve(underlying, componentNode).flatMap {
        case tpe: types.ClassType => Compilation.succeed(ComponentType(tpe))
        case _ => Compilation.fail(ComponentTypeMustContainClass(componentNode))
      }
    }
  }
}
