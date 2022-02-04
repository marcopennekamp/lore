package lore.compiler.assembly.expressions

import lore.compiler.assembly.{AsmChunk, RegisterProvider}
import lore.compiler.core.{CompilationException, Position}
import lore.compiler.poem.{Poem, PoemInstruction, PoemOperation}
import lore.compiler.poem.PoemOperation.PoemOperation
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.{BinaryOperator, UnaryOperator, XaryOperator}
import lore.compiler.types.BasicType

/**
  * The OperationAssembler handles operation assembly for <b>primitive</b> types.
  */
object PrimitiveOperationAssembler {

  def generateUnaryOperation(
    operation: Expression.UnaryOperation,
    valueChunk: AsmChunk,
  )(implicit registerProvider: RegisterProvider): AsmChunk = {
    val c1 = if (operation.tpe == BasicType.Real) convertToReal(operation.value, valueChunk) else valueChunk
    val poemOperation = getPoemOperation(operation.tpe.asInstanceOf[BasicType], operation.operator)
    val target = registerProvider.fresh()
    val instruction = PoemInstruction.UnaryOperation(poemOperation, target, c1.forceResult(operation.position))

    c1 ++ AsmChunk(target, instruction)
  }

  def generateBinaryOperation(
    operation: Expression.BinaryOperation,
    leftChunk: AsmChunk,
    rightChunk: AsmChunk,
  )(implicit registerProvider: RegisterProvider): AsmChunk = {
    val (c1, c2) = if (operation.tpe == BasicType.Real) {
      (convertToReal(operation.left, leftChunk), convertToReal(operation.right, rightChunk))
    } else (leftChunk, rightChunk)

    val poemOperation = getPoemOperation(operation.tpe.asInstanceOf[BasicType], operation.operator)
    val target = registerProvider.fresh()
    buildBinaryInstruction(poemOperation, target, c1, c2, operation.position)
  }

  def generateXaryOperation(
    operation: Expression.XaryOperation,
    operandChunks: Vector[AsmChunk],
  )(implicit registerProvider: RegisterProvider): AsmChunk = {
    val cs = if (operation.tpe == BasicType.Real) {
      operation.expressions.zip(operandChunks).map((convertToReal _).tupled)
    } else operandChunks

    // We can use the same `target` register for all steps as it's legal to consume and overwrite in a single step.
    val poemOperation = getPoemOperation(operation.tpe.asInstanceOf[BasicType], operation.operator)
    val target = registerProvider.fresh()
    cs.tail.foldLeft(cs.head) { (c1, c2) =>
      buildBinaryInstruction(poemOperation, target, c1, c2, operation.position)
    }
  }

  /**
    * Converts the given arithmetic operand to Real if it is an Int.
    */
  private def convertToReal(operand: Expression, operandChunk: AsmChunk)(implicit registerProvider: RegisterProvider): AsmChunk = {
    if (operand.tpe == BasicType.Int) {
      val target = registerProvider.fresh()
      val instruction = PoemInstruction.IntToReal(target, operandChunk.forceResult(operand.position))
      operandChunk ++ AsmChunk(target, instruction)
    } else operandChunk
  }

  private def buildBinaryInstruction(
    poemOperation: PoemOperation,
    target: Poem.Register,
    c1: AsmChunk,
    c2: AsmChunk,
    position: Position,
  )(implicit registerProvider: RegisterProvider): AsmChunk = {
    val instruction = PoemInstruction.BinaryOperation(
      poemOperation,
      target,
      c1.forceResult(position),
      c2.forceResult(position),
    )
    c1 ++ c2 ++ AsmChunk(target, instruction)
  }

  private def getPoemOperation(resultType: BasicType, operator: UnaryOperator): PoemOperation = resultType match {
    case BasicType.Int => operator match {
      case UnaryOperator.Negation => PoemOperation.IntNeg
    }

    case BasicType.Real => operator match {
      case UnaryOperator.Negation => PoemOperation.RealNeg
    }

    case BasicType.Boolean => operator match {
      case UnaryOperator.LogicalNot => PoemOperation.BooleanNot
    }
  }

  private def getPoemOperation(resultType: BasicType, operator: BinaryOperator): PoemOperation = resultType match {
    case BasicType.Int => operator match {
      case BinaryOperator.Addition => PoemOperation.IntAdd
      case BinaryOperator.Subtraction => PoemOperation.IntSub
      case BinaryOperator.Multiplication => PoemOperation.IntMul
      case BinaryOperator.Division => PoemOperation.IntDiv
      case BinaryOperator.Equals => PoemOperation.IntEq
      case BinaryOperator.LessThan => PoemOperation.IntLt
      case BinaryOperator.LessThanEquals => PoemOperation.IntLte
    }

    case BasicType.Real => operator match {
      case BinaryOperator.Addition => PoemOperation.RealAdd
      case BinaryOperator.Subtraction => PoemOperation.RealSub
      case BinaryOperator.Multiplication => PoemOperation.RealMul
      case BinaryOperator.Division => PoemOperation.RealDiv
      case BinaryOperator.Equals => PoemOperation.RealEq
      case BinaryOperator.LessThan => PoemOperation.RealLt
      case BinaryOperator.LessThanEquals => PoemOperation.RealLte
    }

    case BasicType.Boolean => operator match {
      // TODO (assembly): Ensure that Boolean comparisons (including LT/LTE) never reach the assembly phase.
      case BinaryOperator.Equals => throw CompilationException("Boolean equality can be resolved statically.")
    }

    case BasicType.String => operator match {
      case BinaryOperator.Equals => PoemOperation.StringEq
      case BinaryOperator.LessThan => PoemOperation.StringLt
      case BinaryOperator.LessThanEquals => PoemOperation.StringLte
    }
  }

  private def getPoemOperation(resultType: BasicType, operator: XaryOperator): PoemOperation = resultType match {
    case BasicType.Boolean => operator match {
      case XaryOperator.Disjunction => PoemOperation.BooleanOr
      case XaryOperator.Conjunction => PoemOperation.BooleanAnd
    }

    case BasicType.String => operator match {
      case XaryOperator.Concatenation => PoemOperation.StringConcat
    }
  }

}
