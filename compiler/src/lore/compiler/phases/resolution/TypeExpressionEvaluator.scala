package lore.compiler.phases.resolution

import lore.compiler.core.Compilation
import lore.compiler.core.Compilation._
import lore.compiler.feedback.Feedback
import lore.compiler.semantics.scopes.TypeScope
import lore.compiler.syntax.TypeExprNode
import lore.compiler.types._

object TypeExpressionEvaluator {

  case class DuplicateProperty(property: TypeExprNode.ShapePropertyNode) extends Feedback.Error(property) {
    override def message = s"The property ${property.name} is declared twice in the shape type. Shape type properties must be unique."
  }

  def evaluate(expression: TypeExprNode)(implicit typeScope: TypeScope): Compilation.Result[Type] = evaluate(expression, BasicType.Any)

  /**
    * @param default The default type that should be used when a type expression cannot be evaluated.
    */
  def evaluate(expression: TypeExprNode, default: => Type)(implicit typeScope: TypeScope): Compilation.Result[Type] = {
    val rec = (node: TypeExprNode) => evaluate(node, default)
    expression match {
      case TypeExprNode.IdentifierNode(name, position) =>
        // If a named type cannot be resolved, defaulting to Any ensures that types are fully constructed nonetheless.
        // This removes a whole class of compiler bugs that would otherwise occur, such as sum/intersection types being
        // constructed from zero operands, parameter definitions not being resolved due to a type error, functions not
        // being resolved due to an output type error, and so on.
        typeScope.resolve(name, position).withDefault(default)
      case TypeExprNode.IntersectionNode(expressions, _) => expressions.map(rec).simultaneous.map(IntersectionType.construct)
      case TypeExprNode.SumNode(expressions, _) => expressions.map(rec).simultaneous.map(SumType.construct)
      case TypeExprNode.TupleNode(expressions, _) => expressions.map(rec).simultaneous.map(TupleType(_))
      case TypeExprNode.UnitNode(_) => TupleType.UnitType.compiled
      case TypeExprNode.FunctionNode(input, output, _) => (rec(input).map(Type.tupled), rec(output)).simultaneous.map(FunctionType.tupled)
      case TypeExprNode.ListNode(element, _) => rec(element).map(ListType)
      case TypeExprNode.MapNode(key, value, _) => (rec(key), rec(value)).simultaneous.map(MapType.tupled)
      case TypeExprNode.ShapeNode(propertyNodes, _) =>
        propertyNodes
          .filterDuplicates(_.name, DuplicateProperty)
          .flatMap(_.map(evaluateShapeProperty).simultaneous)
          .map(ShapeType.apply)
      case TypeExprNode.SymbolNode(name, _) => SymbolType(name).compiled
    }
  }

  private def evaluateShapeProperty(expression: TypeExprNode.ShapePropertyNode)(implicit typeScope: TypeScope): Compilation.Result[ShapeType.Property] = {
    evaluate(expression.tpe).map(tpe => ShapeType.Property(expression.name, tpe))
  }

}
