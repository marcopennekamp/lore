package lore.compiler.phases.verification

import lore.compiler.core.Compilation.{ToCompilationExtension, Verification}
import lore.compiler.core.{Compilation, Error, Position}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.types.{BasicType, ListType, ProductType, Type}

object ExpressionVerification {
  case class IllegallyTypedExpression(expression: Expression, expectedTypes: Vector[Type]) extends Error(expression) {
    override def message = s"The expression $expression at this position has the illegal type ${expression.tpe}.$expected"
    private def expected: String = {
      if (expectedTypes.nonEmpty) {
        s" We expected one of the following types (or a subtype thereof): ${expectedTypes.mkString(",")}."
      } else ""
    }
  }

  /**
    * Verifies that the type of the given expression is a subtype of one of the expected types.
    */
  def hasSubtype(expression: Expression, supertypes: Type*): Verification = {
    if (!supertypes.exists(expected => expression.tpe <= expected)) {
      Compilation.fail(IllegallyTypedExpression(expression, supertypes.toVector))
    } else Verification.succeed
  }

  /**
    * Verifies that the type of the given expression is a numeric type.
    */
  def isNumeric(expression: Expression): Verification = {
    hasSubtype(expression, BasicType.Int, BasicType.Real)
  }

  /**
    * Verifies that the types of the given expressions are numeric types.
    */
  def areNumeric(expressions: Expression*): Verification = {
    expressions.toVector.map(isNumeric).simultaneous.verification
  }

  /**
    * Verifies that the type of the given expression is a boolean type.
    */
  def isBoolean(expression: Expression): Verification = {
    hasSubtype(expression, BasicType.Boolean)
  }

  /**
    * Verifies that the types of the given expressions are boolean types.
    */
  def areBooleans(expressions: Expression*): Verification = {
    expressions.toVector.map(isBoolean).simultaneous.verification
  }

  case class WrongNumberOfArguments(signature: FunctionSignature, callPos: Position) extends Error(callPos) {
    override def message: String = s"The function/constructor ${signature.name} was called with the wrong number of arguments." +
      s" Expected: ${signature.parameters.size}."
  }

  /**
    * Checks that the given arguments adhere to the given signature.
    *
    * We are assuming that the signature is fixed, so don't use this for multi-functions!
    */
  def adhereToSignature(arguments: Vector[Expression], signature: FunctionSignature, position: Position): Verification = {
    val parameterTypes = signature.parameters.map(_.tpe)
    if (parameterTypes.size != arguments.size) {
      Compilation.fail(WrongNumberOfArguments(signature, position))
    } else {
      parameterTypes.zip(arguments).map { case (parameterType, argument) =>
        hasSubtype(argument, parameterType)
      }.simultaneous.verification
    }
  }

  /**
    * Verifies that the types of the given expressions are numeric and then infers the result type of an arithmetic
    * operation.
    */
  def inferArithmeticOperationType(left: Expression, right: Expression): Compilation[Type] = {
    areNumeric(left, right).map { _ =>
      if (left.tpe == BasicType.Real || right.tpe == BasicType.Real) BasicType.Real else BasicType.Int
    }
  }

  /**
    * Infers the result type of a loop based on the given body expression.
    */
  def inferLoopType(body: Expression): Compilation[Type] = {
    if (body.tpe == ProductType.UnitType) {
      ProductType.UnitType.compiled
    } else {
      ListType(body.tpe).compiled
    }
  }
}
