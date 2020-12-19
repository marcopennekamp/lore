package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, Error, Position}
import lore.compiler.semantics.TypeScope
import lore.compiler.syntax.TypeExprNode
import lore.compiler.types._

object TypeExpressionEvaluator {

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
      case TypeExprNode.MapNode(key, value, _) => (eval(key), eval(value)).simultaneous.map(MapType.tupled)
    }
  }

}
