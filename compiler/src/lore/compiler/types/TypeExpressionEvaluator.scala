package lore.compiler.types

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, Error, Position}
import lore.compiler.semantics.TypeScope
import lore.compiler.syntax.TypeExprNode

object TypeExpressionEvaluator {
  case class ComponentTypeMustContainClass(node: TypeExprNode.ComponentNode) extends Error(node) {
    override def message: String = s"The component type +${node.underlyingName} must contain a class type. ${node.underlyingName} is not a class."
  }

  def evaluate(expression: TypeExprNode)(implicit typeScope: TypeScope): Compilation[Type] = {
    implicit val position: Position = expression.position
    val eval = evaluate _
    expression match {
      case TypeExprNode.IdentifierNode(name, _) => typeScope.resolve(name)
      case TypeExprNode.IntersectionNode(expressions, _) => expressions.map(eval).simultaneous.map(IntersectionType.construct)
      case TypeExprNode.SumNode(expressions, _) => expressions.map(eval).simultaneous.map(SumType.construct)
      case TypeExprNode.ProductNode(expressions, _) => expressions.map(eval).simultaneous.map(ProductType(_))
      case TypeExprNode.UnitNode(_) => ProductType.UnitType.compiled
      case TypeExprNode.ListNode(element, _) => eval(element).map(ListType)
      case TypeExprNode.MapNode(key, value, _) =>
        // Use simultaneous compilation to aggregate errors from both the key and value side. If we used flatMap here,
        // we couldn't report errors about both key and value at the same time (during the same compiler run).
        (eval(key), eval(value)).simultaneous.map(MapType.tupled)
      case componentNode@TypeExprNode.ComponentNode(underlying, _) => typeScope.resolve(underlying).flatMap {
        case tpe: ClassType => ComponentType(tpe).compiled
        case _ => Compilation.fail(ComponentTypeMustContainClass(componentNode))
      }
    }
  }
}
