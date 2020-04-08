package lore.parser

import lore.ast._
import fastparse._
import ScalaWhitespace._

object StatementParser {
  // A helper parser (generator?) that parses sequences of same operators.
  def binary[A, _: P](op: => P[Unit], part: P[ExprNode], node: List[ExprNode] => A): P[A] = {
    P(part ~ (op ~ part).rep(1)).map(aggregate(node))
  }

  // There is only one "true" statement: return.
  def statement[_: P]: P[StmtNode] = P((returnStatement | topLevelExpression) ~ End)
  private def returnStatement[_: P]: P[StmtNode] = P("return" ~/ expression).map(StmtNode.ReturnNode)

  // Parse a handful of top-level expressions before jumping into the deep end.
  def topLevelExpression[_: P]: P[TopLevelExprNode] = P(expression)
  // TODO: Add top-level expression parsers.

  // Parse expressions. Finally!
  def expression[_: P]: P[ExprNode] = P(disjunction)

  // The specific hierarchy implements operator precedence.
  // TODO: This contains a bug. We cannot parse expressions such as a + b - c + d, because the parser looks for either
  //       + or -, but doesn't allow both.
  private def disjunction[_: P]: P[ExprNode] = P(binary("|", conjunction, ExprNode.DisjunctionNode) | conjunction)
  private def conjunction[_: P]: P[ExprNode] = P(binary("&", equality, ExprNode.ConjunctionNode) | equality)
  private def equality[_: P]: P[ExprNode] = P(binary("==", comparison, ExprNode.EqualsNode) | binary("=/=", comparison, ExprNode.NotEqualsNode) | comparison)
  private def comparison[_: P]: P[ExprNode] = {
    // TODO: Comparison operators should only accept two operands!
    P(
      binary("<", additive, ExprNode.LessThanNode) | binary("<=", additive, ExprNode.LessThanEqualsNode) |
        binary(">", additive, ExprNode.GreaterThanNode) | binary(">=", additive, ExprNode.GreaterThanEqualsNode) |
        additive
    )
  }
  private def additive[_: P]: P[ExprNode] = P(binary("+", multiplicative, ExprNode.AdditionNode) | binary("-", multiplicative, ExprNode.SubtractionNode) | multiplicative)
  private def multiplicative[_: P]: P[ExprNode] = P(binary("*", unary, ExprNode.MultiplicationNode) | binary("/", unary, ExprNode.DivisionNode) | unary)
  private def unary[_: P]: P[ExprNode] = P(negation | logicalNot | atom)
  private def negation[_: P]: P[ExprNode] = P("-" ~/ atom).map(ExprNode.NegationNode)
  private def logicalNot[_: P]: P[ExprNode] = P("~" ~/ atom).map(ExprNode.LogicalNotNode)
  private def atom[_: P]: P[ExprNode] = {
    def int: P[ExprNode.IntLiteralNode] = P(LexicalParser.integer).map(ExprNode.IntLiteralNode)
    def real: P[ExprNode.RealLiteralNode] = P(LexicalParser.real).map(ExprNode.RealLiteralNode)
    def booleanLiteral: P[ExprNode.BoolLiteralNode] = P(StringIn("true", "false").!).map(_.toBoolean).map(ExprNode.BoolLiteralNode)
    P(int | real | booleanLiteral)
  }
}
