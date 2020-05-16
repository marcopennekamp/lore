package lore.compiler.phases.parsing

import fastparse.ScalaWhitespace._
import fastparse._
import lore.compiler.ast._

object StatementParser {
  import LexicalParser.identifier
  import Node.withIndex

  // There is only one "true" statement: return.
  def statement[_: P]: P[StmtNode] = P(returnStatement | topLevelExpression)
  private def returnStatement[_: P]: P[StmtNode] = P(Index ~ "return" ~ expression).map(withIndex(StmtNode.ReturnNode))

  // Parse a handful of top-level expressions before jumping into the deep end.
  def topLevelExpression[_: P]: P[TopLevelExprNode] = {
    P(Index ~ (variableDeclaration | assignment | `yield` | continuation | expression)).map(withIndex(identity _))
  }
  private def variableDeclaration[_: P]: P[TopLevelExprNode.VariableDeclarationNode] = {
    P(("const" | "let").! ~ identifier ~ TypeParser.typing.? ~ "=" ~ expression).map { case (qualifier, name, tpe, value) =>
      TopLevelExprNode.VariableDeclarationNode(name, qualifier == "let", tpe, value)
    }
  }
  private def assignment[_: P]: P[TopLevelExprNode] = {
    P(address ~ StringIn("=", "+=", "-=", "*=", "/=").! ~ expression).map { case (address, op, rhs) =>
      op match {
        case "="  => TopLevelExprNode.AssignmentNode(address, rhs)
        case "+=" => TopLevelExprNode.AssignmentNode(address, withIndex(ExprNode.AdditionNode)(rhs.index, address, rhs))
        case "-=" => TopLevelExprNode.AssignmentNode(address, withIndex(ExprNode.SubtractionNode)(rhs.index, address, rhs))
        case "*=" => TopLevelExprNode.AssignmentNode(address, withIndex(ExprNode.MultiplicationNode)(rhs.index, address, rhs))
        case "/=" => TopLevelExprNode.AssignmentNode(address, withIndex(ExprNode.DivisionNode)(rhs.index, address, rhs))
      }
    }
  }

  /**
    * An assignment address is either a plain variable or some property that is accessed on any accessible expression.
    */
  private def address[_: P]: P[ExprNode.AddressNode] = {
    // We can cast to PropertyAccessNode because we set minAccess to 1, which ensures that the parse results in such
    // a node.
    def prop = P(propertyAccess(minAccess = 1)).asInstanceOf[P[ExprNode.PropertyAccessNode]]
    P(Index ~ (prop | variable)).map(withIndex(identity _))
  }

  private def `yield`[_: P]: P[TopLevelExprNode.YieldNode] = P("yield" ~ expression).map(TopLevelExprNode.YieldNode)
  private def continuation[_: P] = {
    def constructorCall = P(("." ~ identifier).? ~ arguments).map(TopLevelExprNode.ConstructorCallNode.tupled)
    def thisCall = P("this" ~ constructorCall)
    def superCall = P("super" ~ constructorCall)
    def constructCall = P("construct" ~ arguments ~ ("with" ~ superCall).?).map(TopLevelExprNode.ConstructNode.tupled)
    P(thisCall | constructCall)
  }

  // Parse expressions. Finally!
  def expression[_: P]: P[ExprNode] = P(Index ~ (ifElse | repeatWhile | iteration | operatorExpression)).map(withIndex(identity _))

  private def ifElse[_: P]: P[ExprNode] = {
    P("if" ~ "(" ~ expression ~ ")" ~ statement ~ ("else" ~ statement).?).map {
      case (condition, onTrue, onFalse) => ExprNode.IfElseNode(condition, onTrue, onFalse.getOrElse(ExprNode.UnitNode))
    }
  }

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
    def extractor = P(Index ~ identifier ~ "in" ~ expression).map(withIndex(ExprNode.ExtractorNode))
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
  private def unary[_: P]: P[ExprNode] = NoCut(P(Index ~ (negation | logicalNot | atom))).map(withIndex(identity _))
  private def negation[_: P]: P[ExprNode] = P("-" ~ atom).map {
    // I don't want to put atom before negation, because I want the parser to handle the minus symbol before considering
    // atoms. However, the way it'd work with a naive implementation of this parser, a negative number would be parsed
    // as NegationNode(IntLiteralNode(x)). Hence we handle this specific case here.
    case ExprNode.IntLiteralNode(x) => ExprNode.IntLiteralNode(-x)
    case ExprNode.RealLiteralNode(x) => ExprNode.RealLiteralNode(-x)
    case expr => ExprNode.NegationNode(expr)
  }
  private def logicalNot[_: P]: P[ExprNode] = P("~" ~ atom).map(ExprNode.LogicalNotNode)

  /**
    * Atomic operands of unary and binary operators.
    *
    * We parse property access here to reduce the amount of backtracking the parser needs to do. Even though all
    * dots are consumed greedily, we still create a chain of PropertyAccessNodes, because that'll make it easier to
    * to compile later.
    */
  private def atom[_: P]: P[ExprNode] = P(propertyAccess(minAccess = 0))

  /**
    * Surrounds an accessible expression with the possibility for property access. This is used by atom AND assignment
    * parsers to apply property access.
    *
    * @param minAccess The minimum number of times that the instance needs to be accessed.
    */
  private def propertyAccess[_: P](minAccess: Int): P[ExprNode] = {
    def propertyAccess = P((Index ~ "." ~ identifier).rep(minAccess))
    P(accessible ~ propertyAccess).map { case (expr, propertyAccesses) =>
      // Create a PropertyAccessNode for every property access or just return the expression if there is
      // no property access.
      propertyAccesses.foldLeft(expr) { case (instance, (index, name)) =>
        withIndex(ExprNode.PropertyAccessNode)(index, instance, name)
      }
    }
  }

  /**
    * All expressions immediately accessible via postfix dot notation.
    */
  private def accessible[_: P]: P[ExprNode] = P(Index ~ (literal | fixedCall | call | variable | block | list | map | enclosed)).map(withIndex(identity _))

  private def literal[_: P]: P[ExprNode] = {
    def real = P(LexicalParser.real).map(ExprNode.RealLiteralNode)
    def int = P(LexicalParser.integer).map(ExprNode.IntLiteralNode)
    def booleanLiteral = P(StringIn("true", "false").!).map(_.toBoolean).map(ExprNode.BoolLiteralNode)
    // Reals have to be parsed before ints so that ints don't consume the portion of the real before the fraction.
    P(real | int | booleanLiteral | LexicalParser.string)
  }
  private def fixedCall[_: P]: P[ExprNode] = P(identifier ~ ".fixed" ~ typeArguments ~ arguments).map(ExprNode.FixedFunctionCallNode.tupled)
  private def call[_: P]: P[ExprNode] = P(identifier ~ ("." ~ identifier).? ~ arguments).map(ExprNode.SimpleCallNode.tupled)
  def arguments[_: P]: P[List[ExprNode]] = P("(" ~ expression.rep(sep = ",") ~ ")").map(_.toList)
  private def typeArguments[_: P]: P[List[TypeExprNode]] = P("[" ~ TypeParser.typeExpression.rep(sep = ",") ~ "]").map(_.toList)
  private def variable[_: P]: P[ExprNode.VariableNode] = P(identifier).map(ExprNode.VariableNode)
  def block[_: P]: P[ExprNode.BlockNode] = {
    def statements = P(statement.repX(0, Space.terminators).map(_.toList))
    P(Index ~ "{" ~ statements ~ "}").map(withIndex(ExprNode.BlockNode))
  }
  private def list[_: P]: P[ExprNode] = P("[" ~ expression.rep(sep = ",") ~ "]").map(_.toList).map(ExprNode.ListNode)
  private def map[_: P]: P[ExprNode] = {
    def keyValue = P(Index ~ expression ~ "->" ~ expression).map(withIndex(ExprNode.KeyValueNode))
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
