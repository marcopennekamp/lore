package lore.compiler.transformation

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression.{BinaryOperator, XaryOperator}
import lore.compiler.semantics.expressions.{Expression, ExpressionIdentityVisitor}
import lore.compiler.transformation.BuiltinsTransformation.ComparisonFunction
import lore.compiler.types.BasicType

/**
  * Ensures that core functions such as areEqual/isLess/isLessThanOrEqual/toString are called at the appropriate times.
  */
class BuiltinsVisitor(implicit registry: Registry, reporter: Reporter) extends ExpressionIdentityVisitor.Simple {

  override def visit(expression: Expression.BinaryOperation)(left: Expression, right: Expression): Expression = expression.operator match {
    case BinaryOperator.Equals =>
      BuiltinsTransformation.transformComparison(ComparisonFunction.AreEqual, BinaryOperator.Equals, left, right, expression.position)

    case BinaryOperator.LessThan =>
      BuiltinsTransformation.transformComparison(ComparisonFunction.IsLessThan, BinaryOperator.LessThan, left, right, expression.position)

    case BinaryOperator.LessThanEquals =>
      BuiltinsTransformation.transformComparison(ComparisonFunction.IsLessThanOrEqual, BinaryOperator.LessThanEquals, left, right, expression.position)

    case _ => super.visit(expression)(left, right)
  }

  override def visit(expression: Expression.XaryOperation)(operands: Vector[Expression]): Expression = expression.operator match {
    case XaryOperator.Concatenation =>
      def transformOperand(operand: Expression) = {
        if (operand.tpe != BasicType.String) {
          CallTransformation.multiFunctionCall("toString", Vector(operand), BasicType.String, operand.position)
        } else operand
      }
      Expression.XaryOperation(XaryOperator.Concatenation, operands.map(transformOperand), BasicType.String, expression.position)

    case _ => super.visit(expression)(operands)
  }

}
