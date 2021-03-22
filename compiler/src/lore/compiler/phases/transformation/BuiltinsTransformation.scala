package lore.compiler.phases.transformation

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, Position}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.BinaryOperator
import lore.compiler.types.BasicType

object BuiltinsTransformation {

  /**
    * Transforms comparison operations (==, !=, <, <=, >, >=) of non-basic types into function calls, invoking the
    * standard functions areEqual, isLessThan, and isLessThanOrEqual. If this comparison is comparing basic types,
    * it instead applies the basic operator for comparison.
    *
    * Results in a compilation error if areEqual, isLessThan, or isLessThanOrEqual doesn't return a boolean value.
    */
  def transformComparison(
    functionName: String, basicOperator: BinaryOperator, left: Expression, right: Expression, position: Position,
  )(implicit registry: Registry): Compilation[Expression] = {
    (left.tpe, right.tpe) match {
      case (_: BasicType, _: BasicType) => Expression.BinaryOperation(basicOperator, left, right, BasicType.Boolean, position).compiled
      case _ => CallTransformation.multiFunctionCall(functionName, Vector(left, right), position).flatMap(CallVerification.ensureOutputType(BasicType.Boolean))
    }
  }

}
