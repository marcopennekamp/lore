package lore.compiler.phases.transformation

import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression.{BinaryOperator, XaryOperator}
import lore.compiler.semantics.expressions.{Expression, ExpressionIdentityVisitor}
import lore.compiler.types.BasicType

/**
  * Ensures that core functions such as areEqual/isLess/isLessThanOrEqual/toString are called at the appropriate times.
  */
class BuiltinsVisitor(implicit registry: Registry) extends ExpressionIdentityVisitor.Compiled {

  override def visit(expression: Expression.BinaryOperation)(left: Expression, right: Expression): Compilation[Expression] = expression.operator match {
    case BinaryOperator.Equals =>
      BuiltinsTransformation.transformComparison("areEqual", BinaryOperator.Equals, left, right, expression.position)

    case BinaryOperator.LessThan =>
      BuiltinsTransformation.transformComparison("isLessThan", BinaryOperator.LessThan, left, right, expression.position)

    case BinaryOperator.LessThanEquals =>
      BuiltinsTransformation.transformComparison("isLessThanOrEqual", BinaryOperator.LessThanEquals, left, right, expression.position)

    case _ => super.visit(expression)(left, right)
  }

  override def visit(expression: Expression.XaryOperation)(operands: Vector[Expression]): Compilation[Expression] = expression.operator match {
    case XaryOperator.Concatenation =>
      def applyToString(operand: Expression) = {
        if (operand.tpe != BasicType.String) {
          CallTransformation
            .multiFunctionCall("toString", Vector(operand), operand.position)
            .flatMap(CallVerification.ensureOutputType(BasicType.String))
        } else operand.compiled
      }

      operands
        .map(applyToString).simultaneous
        .map(transformedOperands => Expression.XaryOperation(XaryOperator.Concatenation, transformedOperands, BasicType.String, expression.position))

    case _ => super.visit(expression)(operands)
  }

}
