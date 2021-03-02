package lore.compiler.phases.typing

import lore.compiler.semantics.expressions.Expression.{BinaryOperator, XaryOperator}
import lore.compiler.semantics.expressions.{Expression, ExpressionIdentityVisitor}

/**
  * Ensures that core functions such as areEqual/isLess/isLessEquals/toString are called at the appropriate times.
  */
class BuiltinsVisitor extends ExpressionIdentityVisitor.Simple {

  // Concatenation:
  /* for {
    transformedExpressions <- expressions.map { expression =>
      if (expression.tpe == BasicType.String) expression.compiled
      else ExpressionBuilder.multiFunctionCall("toString", Vector(expression), expression.position)
    }.simultaneous
  } yield Expression.XaryOperation(XaryOperator.Concatenation, transformedExpressions, BasicType.String, position) */

  override def visit(expression: Expression.BinaryOperation)(left: Expression, right: Expression): Expression = expression.operator match {
    case BinaryOperator.Equals => expression // TODO: areEqual
    case BinaryOperator.LessThan => expression // TODO: isLessThan
    case BinaryOperator.LessThanEquals => expression // TODO: isLessThanEquals
    case _ => super.visit(expression)(left, right)
  }

  override def visit(expression: Expression.XaryOperation)(operands: Vector[Expression]): Expression = expression.operator match {
    case XaryOperator.Concatenation => expression // TODO: toString
    case _ => super.visit(expression)(operands)
  }

}
