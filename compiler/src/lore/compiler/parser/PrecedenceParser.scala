package lore.compiler.parser

import lore.compiler.core.{CompilationException, Fragment, Position}
import lore.compiler.parser.PrecedenceParser.{BinaryOperator, Operator, XaryOperator}
import lore.compiler.syntax.Node
import lore.compiler.syntax.Node.Index

/**
  * Implements the shunting-yard algorithm for arbitrary operators with arbitrary precedence. Supports both binary
  * operators and xary operators.
  */
trait PrecedenceParser { _: Parser =>
  /**
    * Parses a complete sequence of operands and operations into a single "operand", for example a
    * [[lore.compiler.syntax.ExprNode]]. Also easily handles the case where no operators even exist and there is only a
    * single operand.
    *
    * @param operator A parser that should return a [[Operator]] instance based on the operator that was parsed.
    */
  def parseOperationWithPrecedence[Operand <: Node](
    indentation: Int,
    operator: () => Option[Operator[Operand]],
    operand: () => Option[Operand],
  ): Option[Operand] = {
    val (operands, operators) = collectSepSemantic(ws() *> operator() <* wlgi(indentation))(operand())
    val firstOperand = operands.headOption match {
      case option if operands.length <= 1 => return option
      case Some(value) => value
    }

    var operandStack = List[Operand](firstOperand)
    var operatorStack = List[Operator[Operand]]()

    def handleOperator(): Unit = operatorStack.head match {
      // We process N operators of the same precedence with N+1 operands.
      case topOp@XaryOperator(_, _) =>
        val operators = operatorStack.span(_.precedence == topOp.precedence) match {
          case (operators, stack) => operatorStack = stack; operators
        }

        if (operators.exists(_ != operators.head)) {
          throw CompilationException(s"All operators have to be the same for the xary operation to be valid:" +
            s" $operators.")
        }

        if (operandStack.length <= operators.length) {
          throw CompilationException(s"There must be more operands on the operand stack than operators." +
            s" Operators: $operators. Operand stack: $operandStack.")
        }

        // Now construct the node and add it to the output.
        val operands = operandStack.splitAt(operators.length + 1) match {
          // We reverse the list, since the first stack element must be the last of the operand list.
          case (operands, stack) => operandStack = stack; operands.reverse
        }
        val startIndex = operands.head.position.startIndex
        val endIndex = operands.last.position.endIndex
        operandStack = topOp.constructorWithPosition(startIndex, operands.toVector, endIndex) +: operandStack

      // We process one operator with two operands.
      case topOp@BinaryOperator(_, _) =>
        operatorStack = operatorStack.drop(1)

        if (operandStack.length < 2) {
          throw CompilationException(s"The operand stack must contain at least two operands to process a binary" +
            s" operation. Operand stack: $operandStack.")
        }

        // Again, the order of operands needs to be reversed for the AST node.
        val (b, a) = (operandStack.head, operandStack.tail.head)
        operandStack = operandStack.drop(2)
        operandStack = topOp.constructorWithPosition(a.position.startIndex, a, b, b.position.endIndex) +: operandStack
    }

    // Process the expression piece by piece. We handle two iterations of the standard shunting-yard algorithm in one.
    // Due to the order of the input, we first handle the operator case and then the operand case.
    for ((operator, operand) <- operators.zip(operands.tail)) {
      // Handle operator case.
      while (
        operatorStack.nonEmpty && (
          // With xary operators, we only apply them if their precedence is higher than what comes next, and hence
          // they finally need to be applied. This allows us to push a whole sequence of this operator to the stack.
          operatorStack.head.isXary && operatorStack.head.precedence > operator.precedence ||
            // In the binary case, we need to apply the operator from the stack before we push a new operator of the
            // same precedence. Otherwise, we are parsing the expression in a right-associative manner, not
            // left-associative.
            !operatorStack.head.isXary && operatorStack.head.precedence >= operator.precedence
          )
      ) {
        handleOperator()
      }
      operatorStack = operator +: operatorStack

      // Handle operand case.
      operandStack = operand +: operandStack
    }

    while (operatorStack.nonEmpty) {
      handleOperator()
    }

    if (operandStack.length != 1) {
      throw CompilationException("At this point in the shunting-yard algorithm, the operand stack must have exactly one" +
        s" operand: the full expression. Operand stack: $operandStack.")
    }

    operandStack.headOption
  }
}

object PrecedenceParser {
  sealed trait Operator[Operand <: Node] {
    def precedence: Int
    def isXary: Boolean = false
  }

  case class XaryOperator[Operand <: Node](
    precedence: Int,
    constructor: (Vector[Operand], Position) => Operand,
  )(implicit fragment: Fragment) extends Operator[Operand] {
    override def isXary: Boolean = true
    val constructorWithPosition: (Index, Vector[Operand], Index) => Operand = Node.withPositionUntupled(constructor)
  }

  case class BinaryOperator[Operand <: Node](
    precedence: Int,
    constructor: (Operand, Operand, Position) => Operand,
  )(implicit fragment: Fragment) extends Operator[Operand] {
    val constructorWithPosition: (Index, Operand, Operand, Index) => Operand = Node.withPositionUntupled(constructor)
  }
}
