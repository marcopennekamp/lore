package lore.compiler.assembly.functions

import lore.compiler.assembly.{Chunk, RegisterProvider}
import lore.compiler.core.{CompilationException, Position}
import lore.compiler.poem.{Poem, PoemInstruction, PoemOperation}
import lore.compiler.poem.PoemOperation.PoemOperation
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.expressions.Expression.{BinaryOperator, UnaryOperator, XaryOperator}
import lore.compiler.types.{BasicType, Kind, Type}

/**
  * The OperationAssembler handles operation assembly for <b>primitive</b> types.
  */
object PrimitiveOperationAssembler {

  def generateUnaryOperation(
    operation: Expression.UnaryOperation,
    valueChunk: Chunk,
  )(implicit registerProvider: RegisterProvider): Chunk = {
    val domainKind = getDomainKind(operation.value.tpe)
    val c1 = if (domainKind == Kind.Real) convertToReal(operation.value, valueChunk) else valueChunk
    val poemOperation = getPoemOperation(domainKind, operation.operator)
    val target = registerProvider.fresh()
    val instruction = PoemInstruction.UnaryOperation(poemOperation, target, c1.forceResult(operation.position))

    c1 ++ Chunk(target, instruction)
  }

  def generateBinaryOperation(
    operation: Expression.BinaryOperation,
    leftChunk: Chunk,
    rightChunk: Chunk,
  )(implicit registerProvider: RegisterProvider): Chunk = {
    val domainKind = getDomainKind(operation.left.tpe, operation.right.tpe)
    val (c1, c2) = if (domainKind == Kind.Real) {
      (convertToReal(operation.left, leftChunk), convertToReal(operation.right, rightChunk))
    } else (leftChunk, rightChunk)

    val poemOperation = getPoemOperation(domainKind, operation.operator)
    val target = registerProvider.fresh()
    buildBinaryInstruction(poemOperation, target, c1, c2, operation.position)
  }

  def generateXaryOperation(
    operation: Expression.XaryOperation,
    operandChunks: Vector[Chunk],
  )(implicit registerProvider: RegisterProvider): Chunk = {
    val domainKind = getDomainKind(operation.operands.map(_.tpe): _*)
    val cs = if (domainKind == Kind.Real) {
      operation.operands.zip(operandChunks).map((convertToReal _).tupled)
    } else operandChunks

    // We can use the same `target` register for all steps as it's legal to consume and overwrite in a single step.
    val poemOperation = getPoemOperation(domainKind, operation.operator)
    val target = registerProvider.fresh()
    cs.tail.foldLeft(cs.head) { (c1, c2) =>
      buildBinaryInstruction(poemOperation, target, c1, c2, operation.position)
    }
  }

  /**
    * Computes the kind which represents the domain of the operation. This determines the exact operation code used,
    * such as RealAdd if the domain kind is Real, or IntAdd if the domain kind is Int.
    *
    * In niche cases, an operand might be a type variable with a basic type upper bound. For example, we could have a
    * type variable `A <: Real`. This is nonsensical in a practical sense, but still valid Lore code. The
    * transformation phase allows `A` to use `Real` operations, so the operation assembler also has to support this.
    * Hence, we're checking with subtyping instead of type equality to find out the domain kind.
    */
  private def getDomainKind(operandTypes: Type*): Kind = {
    if (operandTypes.forall(_ <= BasicType.Int)) Kind.Int
    else if (operandTypes.forall(t => t <= BasicType.Real || t <= BasicType.Int)) Kind.Real
    else if (operandTypes.forall(_ <= BasicType.Boolean)) Kind.Boolean
    else if (operandTypes.forall(_ <= BasicType.String)) Kind.String
    else if (operandTypes.forall(_.isSymbol)) Kind.Symbol
    else throw CompilationException(s"Invalid operand types combination to compute domain kind: $operandTypes.")
  }

  /**
    * Converts the given arithmetic operand to Real if it is an Int.
    */
  private def convertToReal(operand: Expression, operandChunk: Chunk)(implicit registerProvider: RegisterProvider): Chunk = {
    if (operand.tpe == BasicType.Int) {
      val target = registerProvider.fresh()
      val instruction = PoemInstruction.IntToReal(target, operandChunk.forceResult(operand.position))
      operandChunk ++ Chunk(target, instruction)
    } else operandChunk
  }

  private def buildBinaryInstruction(
    poemOperation: PoemOperation,
    target: Poem.Register,
    c1: Chunk,
    c2: Chunk,
    position: Position,
  )(implicit registerProvider: RegisterProvider): Chunk = {
    val instruction = PoemInstruction.BinaryOperation(
      poemOperation,
      target,
      c1.forceResult(position),
      c2.forceResult(position),
    )
    c1 ++ c2 ++ Chunk(target, instruction)
  }

  private def getPoemOperation(domainKind: Kind, operator: UnaryOperator): PoemOperation = {
    def invalidOperator: Nothing = throw CompilationException(s"Invalid operator $operator for unary $domainKind operation.")

    domainKind match {
      case Kind.Int => operator match {
        case UnaryOperator.Negation => PoemOperation.IntNeg
        case _ => invalidOperator
      }

      case Kind.Real => operator match {
        case UnaryOperator.Negation => PoemOperation.RealNeg
        case _ => invalidOperator
      }

      case Kind.Boolean => operator match {
        case UnaryOperator.LogicalNot => PoemOperation.BooleanNot
        case _ => invalidOperator
      }

      case _ => throw CompilationException(s"Invalid domain kind $domainKind for unary operator $operator.")
    }
  }

  private def getPoemOperation(domainKind: Kind, operator: BinaryOperator): PoemOperation = {
    def invalidOperator: Nothing = throw CompilationException(s"Invalid operator $operator for binary $domainKind operation.")

    domainKind match {
      case Kind.Int => operator match {
        case BinaryOperator.Addition => PoemOperation.IntAdd
        case BinaryOperator.Subtraction => PoemOperation.IntSub
        case BinaryOperator.Multiplication => PoemOperation.IntMul
        case BinaryOperator.Division => PoemOperation.IntDiv
        case BinaryOperator.Equals => PoemOperation.IntEq
        case BinaryOperator.LessThan => PoemOperation.IntLt
        case BinaryOperator.LessThanEquals => PoemOperation.IntLte
        case _ => invalidOperator
      }

      case Kind.Real => operator match {
        case BinaryOperator.Addition => PoemOperation.RealAdd
        case BinaryOperator.Subtraction => PoemOperation.RealSub
        case BinaryOperator.Multiplication => PoemOperation.RealMul
        case BinaryOperator.Division => PoemOperation.RealDiv
        case BinaryOperator.Equals => PoemOperation.RealEq
        case BinaryOperator.LessThan => PoemOperation.RealLt
        case BinaryOperator.LessThanEquals => PoemOperation.RealLte
        case _ => invalidOperator
      }

      case Kind.Boolean => operator match {
        case BinaryOperator.Equals => PoemOperation.BooleanEq
        case _ => invalidOperator
      }

      case Kind.String => operator match {
        case BinaryOperator.Equals => PoemOperation.StringEq
        case BinaryOperator.LessThan => PoemOperation.StringLt
        case BinaryOperator.LessThanEquals => PoemOperation.StringLte
        case _ => invalidOperator
      }

      case Kind.Symbol => operator match {
        case BinaryOperator.Equals => PoemOperation.SymbolEq
        case _ => invalidOperator
      }

      case _ => throw CompilationException(s"Invalid domain kind $domainKind for binary operator $operator.")
    }
  }

  private def getPoemOperation(domainKind: Kind, operator: XaryOperator): PoemOperation = {
    def invalidOperator: Nothing = throw CompilationException(s"Invalid operator $operator for xary $domainKind operation.")

    domainKind match {
      case Kind.Boolean => operator match {
        case XaryOperator.Disjunction => PoemOperation.BooleanOr
        case XaryOperator.Conjunction => PoemOperation.BooleanAnd
        case _ => invalidOperator
      }

      case Kind.String => operator match {
        case XaryOperator.Concatenation => PoemOperation.StringConcat
        case _ => invalidOperator
      }

      case _ => throw CompilationException(s"Invalid domain kind $domainKind for xary operator $operator.")
    }
  }

}
