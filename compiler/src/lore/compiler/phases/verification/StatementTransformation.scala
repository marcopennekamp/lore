package lore.compiler.phases.verification

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, CompilationException, Position}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.{BinaryOperator, XaryOperator}
import lore.compiler.semantics.structures.ClassDefinition
import lore.compiler.types.BasicType

/**
  * Provides functions for transforming statement nodes into expressions.
  */
object StatementTransformation {
  def transformNumericOperation(
    operator: BinaryOperator, left: Expression, right: Expression, position: Position,
  ): Compilation[Expression.BinaryOperation] = {
    ExpressionVerification.inferArithmeticOperationType(left, right).map { tpe =>
      Expression.BinaryOperation(operator, left, right, tpe, position)
    }
  }

  def transformBooleanOperation(operator: XaryOperator, expressions: List[Expression], position: Position): Compilation[Expression.XaryOperation] = {
    ExpressionVerification.areBooleans(expressions: _*).map { _ =>
      Expression.XaryOperation(operator, expressions, BasicType.Boolean, position)
    }
  }

  /**
    * Transforms comparison operations (==, =/=, <, <=, >, >=) of non-basic types into function calls, invoking
    * the standard functions areEqual, isLessThan, and isLessThanOrEqual. If this comparison is comparing basic types,
    * it instead applies the basic operator for comparison.
    */
  def transformComparison(
    functionName: String, basicOperator: BinaryOperator, left: Expression, right: Expression, position: Position,
  )(implicit registry: Registry): Compilation[Expression] = {
    (left.tpe, right.tpe) match {
      case (_: BasicType, _: BasicType) => Expression.BinaryOperation(basicOperator, left, right, BasicType.Boolean, position).compiled
      case _ => ExpressionBuilder.multiFunctionCall(functionName, List(left, right), position).map { call =>
        // TODO: Should this rather be a regular compilation Error?
        if (call.tpe != BasicType.Boolean) {
          throw CompilationException(s"The function $functionName must return a Boolean value.")
        }
        call
      }
    }
  }

  def transformConstructorCall(
    definition: ClassDefinition, qualifier: Option[String], arguments: List[Expression],
  )(implicit position: Position, registry: Registry): Compilation[Expression] = {
    for {
      constructor <- definition.resolveConstructor(qualifier.getOrElse(definition.name))(position)
      _ <- ExpressionVerification.adhereToSignature(arguments, constructor.signature, position)
    } yield Expression.Call(constructor, arguments, position)
  }
}
