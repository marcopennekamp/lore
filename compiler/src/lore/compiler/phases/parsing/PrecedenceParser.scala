package lore.compiler.phases.parsing

import fastparse.ScalaWhitespace._
import fastparse._
import lore.compiler.syntax.Node
import lore.compiler.syntax.Node.Index
import lore.compiler.core.{Fragment, Position}

/**
  * Implements the shunting-yard algorithm for arbitrary operators with arbitrary precedence. Supports both binary
  * operators and xary operators.
  */
object PrecedenceParser {
  sealed trait Operator {
    def precedence: Int
    def isXary: Boolean = false
  }

  case class XaryOperator[Operand <: Node](
    precedence: Int, constructor: (Vector[Operand], Position) => Operand
  )(implicit fragment: Fragment) extends Operator {
    override def isXary: Boolean = true
    val constructorWithIndex: (Index, Vector[Operand]) => Operand = Node.withIndexUntupled(constructor)
  }

  case class BinaryOperator[Operand <: Node](
    precedence: Int, constructor: (Operand, Operand, Position) => Operand
  )(implicit fragment: Fragment) extends Operator {
    val constructorWithIndex: (Index, Operand, Operand) => Operand = Node.withIndexUntupled(constructor)
  }

  /**
    * Parses a complete sequence of operands and operations into a single "operand", for example an expression node.
    * Also easily handles the case where no operators even exist and there is only a single operand.
    *
    * @param operator Although the operators are already define in `operatorMeta`, we need to supply a direct parser to
    *                 fastparse, since it can't take the keys from the run-time map. This should use StringIn or similar
    *                 parsers for performance reasons.
    */
  def parser[Operand <: Node, _: P](
    operator: => P[Unit],
    operand: => P[Operand],
    operatorMeta: Map[String, Operator],
  ): P[Operand] = P(operand ~ (operator.! ~ operand).rep).map { case (left, ops) =>
    var operandStack = List[Operand](left)
    var operatorStack = List[Operator]()

    def handleOperator(): Unit = operatorStack.head match {
      case topOp: XaryOperator[Operand] =>
        // We process N operators of the same precedence with N+1 operands.
        val operators = operatorStack.span(_.precedence == topOp.precedence) match {
          case (operators, stack) => operatorStack = stack; operators
        }
        // All operators have to be the same for the xary construction to be valid.
        assert(operators.forall(_ == operators.head))
        // At this point, we need to have at least one more operand on the stack to work with than operators.
        assert(operandStack.length > operators.length)
        // Now construct the node and add it to the output.
        val operands = operandStack.splitAt(operators.length + 1) match {
          // We reverse the list, since the first stack element must be the last of the operand list.
          case (operands, stack) => operandStack = stack; operands.reverse
        }
        val index = operands.head.position.index
        operandStack = topOp.constructorWithIndex(index, operands.toVector) +: operandStack
      case topOp: BinaryOperator[Operand] =>
        // We process one operator with two operands.
        operatorStack = operatorStack.drop(1) // Actually drop the topOp from the stack.
        // We need at least two operands on the stack to build the operation.
        assert(operandStack.length >= 2)
        // Again, the order of operands needs to be reversed for the AST node.
        val (b, a) = (operandStack.head, operandStack.tail.head)
        operandStack = operandStack.drop(2)
        operandStack = topOp.constructorWithIndex(a.position.index, a, b) +: operandStack
    }

    // Process the expression piece by piece. We handle two iterations of the standard shunting-yard algorithm in one.
    // Due to the order of the input, we first handle the operator case and then the operand case.
    for ((operatorString, operand) <- ops) {
      // Handle operator case.
      val operator = operatorMeta(operatorString)
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

    // Handle all operators that are left on the operator stack.
    while (operatorStack.nonEmpty) {
      handleOperator()
    }

    // At this point, the operand stack should have exactly one node: the full expression.
    assert(operandStack.length == 1)

    // We simply have to return this node from the parser.
    operandStack.head
  }
}
