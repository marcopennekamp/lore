package lore.compiler.transformation

import lore.compiler.feedback.Reporter
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression.{BinaryOperator, XaryOperator}
import lore.compiler.semantics.expressions.{Expression, ExpressionIdentityVisitor}
import lore.compiler.types.BasicType

/**
  * Ensures that core functions such as equal?/less_than?/less_than_equal?/to_string are called at the appropriate times.
  */
class BuiltinsVisitor(implicit registry: Registry, reporter: Reporter) extends ExpressionIdentityVisitor.Simple {

  override def visit(
    expression: Expression.BinaryOperation,
  )(left: Expression, right: Expression): Expression = expression.operator match {
    case BinaryOperator.Equals =>
      BuiltinsTransformation.transformComparison(
        registry.coreDefinitions.equal,
        BinaryOperator.Equals,
        left,
        right,
        expression.position,
      )

    case BinaryOperator.LessThan =>
      BuiltinsTransformation.transformComparison(
        registry.coreDefinitions.less_than,
        BinaryOperator.LessThan,
        left,
        right,
        expression.position,
      )

    case BinaryOperator.LessThanEquals =>
      BuiltinsTransformation.transformComparison(
        registry.coreDefinitions.less_than_equal,
        BinaryOperator.LessThanEquals,
        left,
        right,
        expression.position,
      )

    case _ => super.visit(expression)(left, right)
  }

  override def visit(
    expression: Expression.XaryOperation,
  )(operands: Vector[Expression]): Expression = expression.operator match {
    case XaryOperator.Concatenation =>
      def transformOperand(operand: Expression) = {
        if (operand.tpe != BasicType.String) {
          BuiltinsTransformation.multiFunctionCall(
            registry.coreDefinitions.to_string,
            Vector(operand),
            operand.position,
          )
        } else operand
      }
      Expression.XaryOperation(
        XaryOperator.Concatenation,
        operands.map(transformOperand),
        BasicType.String,
        expression.position,
      )

    case _ => super.visit(expression)(operands)
  }

}
