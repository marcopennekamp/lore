package lore.parser

import lore.ast._
import fastparse._
import ScalaWhitespace._

object StatementParser {
  import LexicalParser.identifier

  // There is only one "true" statement: return.
  def statement[_: P]: P[StmtNode] = P(returnStatement | topLevelExpression)
  private def returnStatement[_: P]: P[StmtNode] = P("return" ~ expression).map(StmtNode.ReturnNode)

  // Parse a handful of top-level expressions before jumping into the deep end.
  def topLevelExpression[_: P]: P[TopLevelExprNode] = P(variableDeclaration | assignment | `yield` | expression)
  private def variableDeclaration[_: P]: P[TopLevelExprNode.VariableDeclarationNode] = {
    P(("const" | "let").! ~ identifier ~ "=" ~ expression).map { case (qualifier, name, value) =>
      TopLevelExprNode.VariableDeclarationNode(name, value, qualifier == "let")
    }
  }
  private def assignment[_: P]: P[TopLevelExprNode.AssignmentNode] = {
    P(address ~ StringIn("=", "+=", "-=", "*=", "/=").! ~ expression).map {
      case (address, "=", rhs) => TopLevelExprNode.AssignmentNode(address, rhs)
      case (address, "+=", rhs) => TopLevelExprNode.AssignmentNode(address, ExprNode.AdditionNode(address.toExpression, rhs))
      case (address, "-=", rhs) => TopLevelExprNode.AssignmentNode(address, ExprNode.SubtractionNode(address.toExpression, rhs))
      case (address, "*=", rhs) => TopLevelExprNode.AssignmentNode(address, ExprNode.MultiplicationNode(address.toExpression, rhs))
      case (address, "/=", rhs) => TopLevelExprNode.AssignmentNode(address, ExprNode.DivisionNode(address.toExpression, rhs))
    }
  }
  private def address[_: P]: P[TopLevelExprNode.AddressNode] = {
    P(identifier ~~ ("." ~~ identifier).rep).map { case (s1, strings) => TopLevelExprNode.AddressNode(s1 +: strings.toList) }
  }
  private def `yield`[_: P]: P[TopLevelExprNode.YieldNode] = P("yield" ~ expression).map(TopLevelExprNode.YieldNode)

  // Parse expressions. Finally!
  def expression[_: P]: P[ExprNode] = P(ifElse | repeatWhile | iteration | operatorExpression)

  private def ifElse[_: P]: P[ExprNode] = {
    P("if" ~ "(" ~ expression ~ ")" ~ statement ~ ("else" ~ statement).?).map {
      case (condition, onTrue, onFalse) => ExprNode.IfElseNode(condition, onTrue, onFalse.getOrElse(ExprNode.UnitNode))
    }
  }

  //   case class RepeatWhileNode(condition: ExprNode, body: StmtNode, deferCheck: Boolean) extends ExprNode // TODO: Write parser.

  private def repeatWhile[_: P]: P[ExprNode.RepeatWhileNode] = {
    def whileCond = P("while" ~ "(" ~ expression ~ ")")
    def checkBefore = P(whileCond ~ statement).map {
      case (cond, body) => ExprNode.RepeatWhileNode(cond, body, deferCheck = false)
    }
    def checkAfter = P(statement ~ whileCond).map {
      case (body, cond) => ExprNode.RepeatWhileNode(cond, body, deferCheck = true)
    }
    P("repeat" ~ (checkBefore | checkAfter))
  }

  private def iteration[_: P]: P[ExprNode.IterationNode] = {
    def extractor = P(identifier ~ "in" ~ expression).map(ExprNode.ExtractorNode.tupled)
    P("for" ~ "(" ~ extractor.rep(1, sep = ",") ~ ")" ~ statement).map {
      case (extractors, stat) => ExprNode.IterationNode(extractors.toList, stat)
    }
  }

  def operatorExpression[_: P]: P[ExprNode] = {
    import PrecedenceParser._
    PrecedenceParser.parser(
      operator = StringIn("|", "&", "==", "=/=", "<", "<=", ">", ">=", "+", "-", "*", "/"),
      operand = unary,
      operatorMeta = Map(
        "|" -> XaryOperator(1, ExprNode.DisjunctionNode),
        "&" -> XaryOperator(2, ExprNode.ConjunctionNode),
        "==" -> BinaryOperator(3, ExprNode.EqualsNode),
        "=/=" -> BinaryOperator(3, ExprNode.NotEqualsNode),
        "<" -> BinaryOperator(4, ExprNode.LessThanNode),
        "<=" -> BinaryOperator(4, ExprNode.LessThanEqualsNode),
        ">" -> BinaryOperator(4, ExprNode.GreaterThanNode),
        ">=" -> BinaryOperator(4, ExprNode.GreaterThanEqualsNode),
        "+" -> BinaryOperator(5, ExprNode.AdditionNode),
        "-" -> BinaryOperator(5, ExprNode.SubtractionNode),
        "*" -> BinaryOperator(6, ExprNode.MultiplicationNode),
        "/" -> BinaryOperator(6, ExprNode.DivisionNode),
      ),
    )
  }

  // We apply NoCut here to allow the parser to backtrack if it doesn't find a multiplication/addition operator, while
  // still allowing cuts inside of unary applications and atoms.
  private def unary[_: P]: P[ExprNode] = NoCut(P(negation | logicalNot | atom))
  private def negation[_: P]: P[ExprNode] = P("-" ~ atom).map {
    // I don't want to put atom before negation, because I want the parser to handle the minus symbol before considering
    // atoms. However, the way it'd work with a naive implementation of this parser, a negative number would be parsed
    // as NegationNode(IntLiteralNode(x)). Hence we handle this specific case here.
    case ExprNode.IntLiteralNode(x) => ExprNode.IntLiteralNode(-x)
    case ExprNode.RealLiteralNode(x) => ExprNode.RealLiteralNode(-x)
    case expr => ExprNode.NegationNode(expr)
  }
  private def logicalNot[_: P]: P[ExprNode] = P("~" ~ atom).map(ExprNode.LogicalNotNode)

  private def atom[_: P]: P[ExprNode] = P(literal | accessiblePostfix)
  private def literal[_: P]: P[ExprNode] = {
    def real = P(LexicalParser.real).map(ExprNode.RealLiteralNode)
    def int = P(LexicalParser.integer).map(ExprNode.IntLiteralNode)
    def booleanLiteral = P(StringIn("true", "false").!).map(_.toBoolean).map(ExprNode.BoolLiteralNode)
    // Reals have to be parsed before ints so that ints don't consume the portion of the real before the fraction.
    P(real | int | booleanLiteral | LexicalParser.string)
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
  private def accessible[_: P]: P[ExprNode] = P(fixedCall | call | variable | block | list | map | enclosed)

  private def fixedCall[_: P]: P[ExprNode] = P(identifier ~ ".fixed" ~ typeArguments ~ arguments).map(ExprNode.FixedFunctionCallNode.tupled)
  private def call[_: P]: P[ExprNode] = P(identifier ~ ("." ~ identifier).? ~ arguments).map(ExprNode.CallNode.tupled)
  def arguments[_: P]: P[List[ExprNode]] = P("(" ~ expression.rep(sep = ",") ~ ")").map(_.toList)
  private def typeArguments[_: P]: P[List[TypeExprNode]] = P("[" ~ TypeParser.typeExpression.rep(sep = ",") ~ "]").map(_.toList)
  private def variable[_: P]: P[ExprNode] = P(identifier).map(ExprNode.VariableNode)
  def block[_: P]: P[ExprNode] = P("{" ~ statement.repX(0, Space.terminators) ~ "}").map(_.toList).map(ExprNode.BlockNode)
  private def list[_: P]: P[ExprNode] = P("[" ~ (expression ~ ("," ~ expression).rep).? ~ "]").map {
    case None => ExprNode.ListNode(List.empty)
    case Some((left, expressions)) => ExprNode.ListNode(left +: expressions.toList)
  }
  private def map[_: P]: P[ExprNode] = {
    def keyValue = P(expression ~ "->" ~ expression).map(ExprNode.KeyValueNode.tupled)
    P("%{" ~ keyValue.rep(sep = ",") ~ "}").map(_.toList).map(ExprNode.MapNode)
  }

  /**
    * Parses both enclosed expressions and tuples using the same parser. If the number of expressions is exactly one,
    * it's simply an enclosed expression. Otherwise, it is a tuple.
    */
  private def enclosed[_: P]: P[ExprNode] = {
    P("(" ~ (expression ~ ("," ~ expression).rep).? ~ ")").map {
      case None => ExprNode.UnitNode
      case Some((expr, Seq())) => expr
      case Some((left, expressions)) => ExprNode.TupleNode(left +: expressions.toList)
    }
  }
}
