package lore.compiler.parser

import lore.compiler.core.{CompilationException, Fragment, Position}
import lore.compiler.feedback.ParserFeedback
import lore.compiler.parser.PrecedenceParser.{BinaryOperator, Operator, XaryOperator}
import lore.compiler.syntax.{Node, TkDedent, TkIndent, TkNewline, Token}
import lore.compiler.syntax.Node.Index

/**
  * Implements the shunting-yard algorithm for arbitrary operators with arbitrary precedence. Supports both binary
  * operators and xary operators.
  */
trait PrecedenceParser { _: Parser =>
  /**
    * Parses a complete sequence of operands and operations into a single "operand", for example a
    * [[lore.compiler.syntax.ExprNode]], using lookahead on the operator. `operator` and `operand` should report proper
    * errors on failure.
    *
    * The parser allows expressions to span multiple lines without altering precedence by handling newlines and
    * indentation. Newlines are consumed before an operator (after establishing lookahead) or operand (except the first
    * operand) and the current indentation is counted. A newline can only be consumed if the current indentation is
    * greater than zero or if an indentation immediately follows the newline. Otherwise, a newline is interpreted as the
    * end of the expression.
    *
    * The parser ensures that the expression is properly closed with an appropriate amount of dedents.
    */
  def parseOperationWithPrecedence[Operand <: Node](
    isOperator: Token => Boolean,
    operator: => Result[Operator[Operand]],
    operand: => Result[Operand],
  ): Result[Operand] = {
    val (operands, operators) = collectOperandsAndOperators(isOperator, operator, operand).getOrElse(return Failure)
    if (operands.length == 1) return operands.head.success

    var operandStack = List[Operand](operands.head)
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

    operandStack.head.success
  }

  /**
    * See remarks in [[parseOperationWithPrecedence]] for more information about the newline and indentation handling
    * implemented by this function.
    */
  private def collectOperandsAndOperators[Operand <: Node](
    isOperator: Token => Boolean,
    operator: => Result[Operator[Operand]],
    operand: => Result[Operand],
  ): Result[(Vector[Operand], Vector[Operator[Operand]])] = {
    val firstOperand = operand.getOrElse(return Failure)

    var indentation = 0
    var operands = Vector(firstOperand)
    var operators = Vector.empty[Operator[Operand]]

    def hasNextOperator: Boolean =
      peekIsWithPossibleIndent(isOperator) ||
        indentation > 0 && peekIs[TkNewline] && isOperator(peek(2))

    def handleIndent(): Unit = {
      val isIndented = openOptionalIndentation()
      if (isIndented) indentation += 1
    }

    // Handling dedents at the end of each operator/operand allows complex expressions to fall back to lower indentation
    // levels, for example:
    //    1 +
    //      2 + 3 +
    //        4 * 5 +
    //      6
    def handleDedents(): Unit = {
      if (indentation == 0) return
      if (!peekIs[TkNewline] || !peekIs[TkDedent](2)) return

      skip() // Newline
      while (indentation > 0 && peekIs[TkDedent]) {
        skip()
        indentation -= 1
      }
    }

    while (hasNextOperator) {
      // Handle possible newline/indentation of the operator. `hasNextOperator` ensures that the newline is valid. Note
      // that `handleIndent` cannot handle the newline in all cases because the newline might be valid without an
      // indent.
      consumeIf[TkNewline]
      handleIndent()
      operators :+= operator.getOrElse(return Failure)
      handleDedents()

      // Handle possible newline/indentation of the operand. We need to ensure that the newline doesn't terminate the
      // expression. In the operator case, `hasNextOperator` covers it, but here we can report a nice error to the user.
      if (peekIs[TkNewline]) {
        if (indentation == 0 && !peekIs[TkIndent](2)) {
          // TODO (syntax): Report error: Operand expected, but expression was terminated by a newline.
          return Failure
        }

        // Note that, again, `handleIndent` cannot handle the newline in case it's not followed by an indent, so we
        // need to skip it manually.
        skip()
        handleIndent()
      }
      operands :+= operand.getOrElse(return Failure)
      handleDedents()
    }

    if (indentation > 0) {
      reporter.report(ParserFeedback.TokenExpected[TkDedent](peek.position))
      return Failure
    }

    (operands, operators).success
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
