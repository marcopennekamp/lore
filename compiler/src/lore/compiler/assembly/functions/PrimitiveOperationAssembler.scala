package lore.compiler.assembly.functions

import lore.compiler.assembly.{AsmChunk, RegisterProvider}
import lore.compiler.core.{CompilationException, Position}
import lore.compiler.poem.{Poem, PoemInstruction, PoemOperation}
import lore.compiler.poem.PoemOperation.PoemOperation
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.{BinaryOperator, UnaryOperator, XaryOperator}
import lore.compiler.types.{BasicType, Type}

/**
  * The OperationAssembler handles operation assembly for <b>primitive</b> types.
  */
object PrimitiveOperationAssembler {

  def generateUnaryOperation(
    operation: Expression.UnaryOperation,
    valueChunk: AsmChunk,
  )(implicit registerProvider: RegisterProvider): AsmChunk = {
    val domainType = getDomainType(operation.value.tpe)
    val c1 = if (domainType == BasicType.Real) convertToReal(operation.value, valueChunk) else valueChunk
    val poemOperation = getPoemOperation(domainType, operation.operator)
    val target = registerProvider.fresh()
    val instruction = PoemInstruction.UnaryOperation(poemOperation, target, c1.forceResult(operation.position))

    c1 ++ AsmChunk(target, instruction)
  }

  def generateBinaryOperation(
    operation: Expression.BinaryOperation,
    leftChunk: AsmChunk,
    rightChunk: AsmChunk,
  )(implicit registerProvider: RegisterProvider): AsmChunk = {
    val domainType = getDomainType(operation.left.tpe, operation.right.tpe)
    val (c1, c2) = if (domainType == BasicType.Real) {
      (convertToReal(operation.left, leftChunk), convertToReal(operation.right, rightChunk))
    } else (leftChunk, rightChunk)

    val poemOperation = getPoemOperation(domainType, operation.operator)
    val target = registerProvider.fresh()
    buildBinaryInstruction(poemOperation, target, c1, c2, operation.position)
  }

  def generateXaryOperation(
    operation: Expression.XaryOperation,
    operandChunks: Vector[AsmChunk],
  )(implicit registerProvider: RegisterProvider): AsmChunk = {
    val domainType = getDomainType(operation.expressions.map(_.tpe): _*)
    val cs = if (domainType == BasicType.Real) {
      operation.expressions.zip(operandChunks).map((convertToReal _).tupled)
    } else operandChunks

    // We can use the same `target` register for all steps as it's legal to consume and overwrite in a single step.
    val poemOperation = getPoemOperation(domainType, operation.operator)
    val target = registerProvider.fresh()
    cs.tail.foldLeft(cs.head) { (c1, c2) =>
      buildBinaryInstruction(poemOperation, target, c1, c2, operation.position)
    }
  }

  /**
    * Computes the basic type which represents the domain of the operation. This determines the exact operation code
    * used, such as RealAdd if the domain type is Real, or IntAdd if the domain type is Int.
    *
    * In niche cases, an operand might be a type variable with a basic type upper bound. For example, we could have a
    * type variable `A <: Real`. This is nonsensical in a practical sense, but still valid Lore code. The
    * transformation phase allows `A` to use `Real` operations, so the operation assembler also has to support this.
    * Hence, we're checking with subtyping instead of type equality to find out the domain type.
    */
  private def getDomainType(operandTypes: Type*): BasicType = {
    if (operandTypes.forall(_ <= BasicType.Boolean)) BasicType.Boolean
    else if (operandTypes.forall(_ <= BasicType.String)) BasicType.String
    else if (operandTypes.forall(_ <= BasicType.Int)) BasicType.Int
    else if (operandTypes.forall(t => t <= BasicType.Real || t <= BasicType.Int)) BasicType.Real
    else throw CompilationException(s"Invalid operand types combination to compute domain type: $operandTypes.")
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

  private def getPoemOperation(domainType: BasicType, operator: UnaryOperator): PoemOperation = {
    def invalidOperator: Nothing = throw CompilationException(s"Invalid operator $operator for unary $domainType operation.")

    domainType match {
      case BasicType.Int => operator match {
        case UnaryOperator.Negation => PoemOperation.IntNeg
        case _ => invalidOperator
      }

      case BasicType.Real => operator match {
        case UnaryOperator.Negation => PoemOperation.RealNeg
        case _ => invalidOperator
      }

      case BasicType.Boolean => operator match {
        case UnaryOperator.LogicalNot => PoemOperation.BooleanNot
        case _ => invalidOperator
      }

      case _ => throw CompilationException(s"Invalid domain type $domainType for unary operator $operator.")
    }
  }

  private def getPoemOperation(domainType: BasicType, operator: BinaryOperator): PoemOperation = {
    def invalidOperator: Nothing = throw CompilationException(s"Invalid operator $operator for binary $domainType operation.")

    domainType match {
      case BasicType.Int => operator match {
        case BinaryOperator.Addition => PoemOperation.IntAdd
        case BinaryOperator.Subtraction => PoemOperation.IntSub
        case BinaryOperator.Multiplication => PoemOperation.IntMul
        case BinaryOperator.Division => PoemOperation.IntDiv
        case BinaryOperator.Equals => PoemOperation.IntEq
        case BinaryOperator.LessThan => PoemOperation.IntLt
        case BinaryOperator.LessThanEquals => PoemOperation.IntLte
        case _ => invalidOperator
      }

      case BasicType.Real => operator match {
        case BinaryOperator.Addition => PoemOperation.RealAdd
        case BinaryOperator.Subtraction => PoemOperation.RealSub
        case BinaryOperator.Multiplication => PoemOperation.RealMul
        case BinaryOperator.Division => PoemOperation.RealDiv
        case BinaryOperator.Equals => PoemOperation.RealEq
        case BinaryOperator.LessThan => PoemOperation.RealLt
        case BinaryOperator.LessThanEquals => PoemOperation.RealLte
        case _ => invalidOperator
      }

      case BasicType.Boolean => operator match {
        // TODO (assembly): Ensure that Boolean comparisons (including LT/LTE) never reach the assembly phase.
        case BinaryOperator.Equals => throw CompilationException("Boolean equality can be resolved statically.")
        case _ => invalidOperator
      }

      case BasicType.String => operator match {
        case BinaryOperator.Equals => PoemOperation.StringEq
        case BinaryOperator.LessThan => PoemOperation.StringLt
        case BinaryOperator.LessThanEquals => PoemOperation.StringLte
        case _ => invalidOperator
      }

      case _ => throw CompilationException(s"Invalid domain type $domainType for binary operator $operator.")
    }
  }

  private def getPoemOperation(domainType: BasicType, operator: XaryOperator): PoemOperation = {
    def invalidOperator: Nothing = throw CompilationException(s"Invalid operator $operator for xary $domainType operation.")

    domainType match {
      case BasicType.Boolean => operator match {
        case XaryOperator.Disjunction => PoemOperation.BooleanOr
        case XaryOperator.Conjunction => PoemOperation.BooleanAnd
        case _ => invalidOperator
      }

      case BasicType.String => operator match {
        case XaryOperator.Concatenation => PoemOperation.StringConcat
        case _ => invalidOperator
      }

      case _ => throw CompilationException(s"Invalid domain type $domainType for xary operator $operator.")
    }
  }

}
