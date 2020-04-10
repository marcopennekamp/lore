package lore.parser

import lore.ast._
import fastparse._
import ScalaWhitespace._

object StatementParser {
  import LexicalParser.identifier

  // A helper parser that parses chains of possibly varying operators.
  def chain[_: P](op: => P[Unit], part: => P[ExprNode], opsToNodes: Map[String, (ExprNode, ExprNode) => ExprNode]): P[ExprNode] = {
    P(part ~ (op.! ~ part).rep(1)).map {
      case (left, expressions) =>
        expressions.foldLeft(left){ case (left, (op, right)) =>
          opsToNodes(op)(left, right)
        }
    }
  }

  // There is only one "true" statement: return.
  def statement[_: P]: P[StmtNode] = P(returnStatement | topLevelExpression)
  private def returnStatement[_: P]: P[StmtNode] = P("return" ~ expression).map(StmtNode.ReturnNode)

  // Parse a handful of top-level expressions before jumping into the deep end.
  def topLevelExpression[_: P]: P[TopLevelExprNode] = P(variableDeclaration | assignment | `yield` | expression)
  private def variableDeclaration[_: P]: P[TopLevelExprNode.VariableDeclarationNode] = {
    P(("const" | "let").! ~ identifier ~ "=" ~ expression).map { case (qualifier, name, value) =>
      TopLevelExprNode.VariableDeclarationNode(name, value, qualifier == "const")
    }
  }
  private def assignment[_: P]: P[TopLevelExprNode.AssignmentNode] = P(address ~ "=" ~ expression).map(TopLevelExprNode.AssignmentNode.tupled)
  private def address[_: P]: P[TopLevelExprNode.AddressNode] = {
    P(identifier ~~ ("." ~~ identifier).rep).map { case (s1, strings) => TopLevelExprNode.AddressNode(s1 +: strings.toList) }
  }
  private def `yield`[_: P]: P[TopLevelExprNode.YieldNode] = P("yield" ~ expression).map(TopLevelExprNode.YieldNode)

  // Parse expressions. Finally!
  def expression[_: P]: P[ExprNode] = P(ifElse | disjunction)

  private def ifElse[_: P]: P[ExprNode] = {
    P("if" ~ "(" ~ expression ~ ")" ~ statement ~ ("else" ~ statement).?).map {
      case (condition, onTrue, onFalse) => ExprNode.IfElseNode(condition, onTrue, onFalse.getOrElse(ExprNode.UnitNode))
    }
  }

  // The specific hierarchy implements operator precedence.
  private def disjunction[_: P]: P[ExprNode] = P(xaryList("|", conjunction, ExprNode.DisjunctionNode) | conjunction)
  private def conjunction[_: P]: P[ExprNode] = P(xaryList("&", equality, ExprNode.ConjunctionNode) | equality)
  private def equality[_: P]: P[ExprNode] = P(binary("==", comparison, ExprNode.EqualsNode) | binary("=/=", comparison, ExprNode.NotEqualsNode) | comparison)
  private def comparison[_: P]: P[ExprNode] = {
    P(
      binary("<", additive, ExprNode.LessThanNode) | binary("<=", additive, ExprNode.LessThanEqualsNode) |
        binary(">", additive, ExprNode.GreaterThanNode) | binary(">=", additive, ExprNode.GreaterThanEqualsNode) |
        additive
    )
  }
  private def additive[_: P]: P[ExprNode] = {
    P(chain("+" | "-", multiplicative, Map("+" -> ExprNode.AdditionNode, "-" -> ExprNode.SubtractionNode)) | multiplicative)
  }
  private def multiplicative[_: P]: P[ExprNode] = {
    P(chain("*" | "/", unary, Map("*" -> ExprNode.MultiplicationNode, "/" -> ExprNode.DivisionNode)) | unary)
  }

  // We apply NoCut here to allow the parser to backtrack if it doesn't find a multiplication/addition operator, while
  // still allowing cuts inside of unary applications and atoms.
  private def unary[_: P]: P[ExprNode] = NoCut(P(negation | logicalNot | atom))
  private def negation[_: P]: P[ExprNode] = P("-" ~ atom).map(ExprNode.NegationNode)
  private def logicalNot[_: P]: P[ExprNode] = P("~" ~ atom).map(ExprNode.LogicalNotNode)
  private def atom[_: P]: P[ExprNode] = {
    P(literal | accessiblePostfix)
  }
  private def literal[_: P]: P[ExprNode] = {
    def int = P(LexicalParser.integer).map(ExprNode.IntLiteralNode)
    def real = P(LexicalParser.real).map(ExprNode.RealLiteralNode)
    def booleanLiteral = P(StringIn("true", "false").!).map(_.toBoolean).map(ExprNode.BoolLiteralNode)
    P(int | real | booleanLiteral | LexicalParser.string)
  }

  private def accessiblePostfix[_: P]: P[ExprNode] = {
    def propertyAccess = P(("." ~ identifier).rep)
    P(accessible ~ propertyAccess).map { case (instance, propertyNames) =>
      if (propertyNames.nonEmpty) {
        ExprNode.PropertyAccessNode(instance, propertyNames.toList)
      } else {
        instance
      }
    }
  }

  /**
    * All expressions immediately accessible via postfix dot notation.
    */
  private def accessible[_: P]: P[ExprNode] = P(variable | enclosed | block)
  private def variable[_: P]: P[ExprNode] = P(identifier).map(ExprNode.VariableNode)
  private def enclosed[_: P]: P[ExprNode] = P("(" ~ expression ~ ")")
  private def block[_: P]: P[ExprNode] = P("{" ~ statement.repX(0, Space.terminators) ~ "}").map(_.toList).map(ExprNode.BlockNode)
}
