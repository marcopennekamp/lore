package lore.compiler.typing

import lore.compiler.feedback.{Reporter, TypingFeedback}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.expressions.BinaryOperator
import lore.compiler.semantics.expressions.typed.Expression
import lore.compiler.semantics.expressions.typed.Expression.{BinaryOperation, XaryOperation}
import lore.compiler.semantics.expressions.untyped.UntypedExpression
import lore.compiler.semantics.expressions.untyped.UntypedExpression.{UntypedBinaryOperation, UntypedXaryOperation}
import lore.compiler.types.{BasicType, ListType, SumType, Type}
import lore.compiler.typing.Synthesizer.infer
import lore.compiler.utils.CollectionExtensions.{OptionVectorExtension, Tuple2OptionExtension}

object OperationTyping {

  /**
    * Checks that `operand` is an Int or Real. The Int check is attempted first to get the narrowest possible type.
    */
  def checkArithmeticOperand(
    operand: UntypedExpression,
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    Checker.attempt(operand, BasicType.Int, context)._1.orElse {
      Checker.check(operand, BasicType.Real, context)
    }
  }

  /**
    * Returns the result type of an arithmetic operation with `typedOperands`. If all operands are typed as Int, the
    * result type is also Int, and Real otherwise.
    */
  def getArithmeticResultType(typedOperands: Expression*): Type = {
    if (typedOperands.map(_.tpe).forall(_ == BasicType.Int)) BasicType.Int else BasicType.Real
  }

  def inferArithmeticOperation(
    operation: UntypedBinaryOperation,
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    checkArithmeticOperand(operation.operand1, context).flatMap { case (typedOperand1, context2) =>
      checkArithmeticOperand(operation.operand2, context2).mapFirst { typedOperand2 =>
        val resultType = OperationTyping.getArithmeticResultType(typedOperand1, typedOperand2)
        BinaryOperation(operation.operator, typedOperand1, typedOperand2, resultType, operation.position)
      }
    }
  }

  /**
    * Builds a typed comparison operation (==, !=, <, <=, >, >=) from the given typed operands, invoking the standard
    * functions `equal?`, `less_than?`, and `less_than_equal?` for complex comparisons.
    */
  def inferComparison(
    operation: UntypedBinaryOperation,
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    Synthesizer.infer(operation.operand1, context).flatMap { case (typedOperand1, context2) =>
      infer(operation.operand2, context2).flatMapFirst { typedOperand2 =>
        buildComparison(operation, typedOperand1, typedOperand2)
      }
    }
  }

  private def buildComparison(
    operation: UntypedBinaryOperation,
    operand1: Expression,
    operand2: Expression,
  )(implicit registry: Registry, reporter: Reporter): Option[Expression] = {
    val cmf = operation.operator match {
      case BinaryOperator.Equals => registry.coreDefinitions.equal
      case BinaryOperator.LessThan => registry.coreDefinitions.less_than
      case BinaryOperator.LessThanEquals => registry.coreDefinitions.less_than_equal
    }

    // The following type combinations can be compared by specialized instructions: `(Int | Real, Int | Real)`,
    // `(Boolean, Boolean)` (equality only), `(String, String)`, and `(Symbol, Symbol)` (equality only). Boolean and
    // symbol order is included in the default implementation of `lore.core.less_than?`, but not available as a
    // specialized instruction.
    val hasSpecializedInstruction = (operand1.tpe, operand2.tpe) match {
      case (t1: BasicType, t2: BasicType) if t1.isNumeric && t2.isNumeric => true
      case (BasicType.Boolean, BasicType.Boolean) => cmf == registry.coreDefinitions.equal
      case (BasicType.String, BasicType.String) => true
      case (t1, t2) if t1.isSymbol && t2.isSymbol => cmf == registry.coreDefinitions.equal
      case _ => false
    }

    if (hasSpecializedInstruction) {
      Some(BinaryOperation(operation.operator, operand1, operand2, BasicType.Boolean, operation.position))
    } else {
      CoreBuilder.multiFunctionCall(cmf, Vector(operand1, operand2), operation.position)
    }
  }

  def inferAppend(
    operation: UntypedBinaryOperation,
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    Synthesizer.infer(operation.operand1, context).flatMap { case (typedCollection, context2) =>
      // The appended element's type might need to be informed by the collection's type, for example when the
      // collection is a list of functions and the appended element is a lambda function without type annotations.
      // Hence, we first attempt to check the appended element with the expected element type. This is not always
      // valid, though, because appending might widen the type of the list. When checking fails, we thus need to
      // default to inference.
      def checkAppendedElement(expectedElementType: Type): Option[InferenceResult] = {
        Checker.attempt(operation.operand2, expectedElementType, context2)._1.orElse(infer(operation.operand2, context2))
      }

      typedCollection.tpe match {
        case ListType(elementType) =>
          checkAppendedElement(elementType).mapFirst { typedElement =>
            val resultType = ListType(SumType.construct(elementType, typedElement.tpe))
            BinaryOperation(operation.operator, typedCollection, typedElement, resultType, operation.position)
          }

        case _ =>
          reporter.report(TypingFeedback.Append.ListExpected(typedCollection, operation))
          None
      }
    }
  }

  /**
    * Infers the concatenation `operation`, adding a `lore.core.to_string` call for all operands that aren't already
    * typed as strings.
    */
  def inferConcatenation(
    operation: UntypedXaryOperation,
    context: InferenceContext,
  )(implicit registry: Registry, reporter: Reporter): Option[InferenceResult] = {
    Synthesizer.infer(operation.operands, context).flatMapFirst { typedOperands =>
      stringifyOperands(typedOperands).map { stringifiedOperands =>
        XaryOperation(
          operation.operator,
          stringifiedOperands,
          BasicType.String,
          operation.position,
        )
      }
    }
  }

  private def stringifyOperands(operands: Vector[Expression])(
    implicit registry: Registry,
    reporter: Reporter,
  ): Option[Vector[Expression]] = {
    operands.map { typedOperand =>
      if (typedOperand.tpe != BasicType.String) {
        CoreBuilder.multiFunctionCall(
          registry.coreDefinitions.to_string,
          Vector(typedOperand),
          typedOperand.position,
        )
      } else Some(typedOperand)
    }.sequence
  }

}
