package lore.compiler.parsing

import fastparse._
import lore.compiler.core.{Fragment, Position}
import lore.compiler.parsing.LexicalParser.identifier
import lore.compiler.syntax._

/**
  * @param whitespace This can be manually specified to disable newlines in whitespace.
  */
class ExpressionParser(nameParser: NameParser)(implicit fragment: Fragment, whitespace: P[Any] => P[Unit]) {
  private val typeParser = new TypeParser(nameParser)
  private val stringParser = new StringParser(nameParser, this)
  private lazy val singleLineParser = new ExpressionParser(nameParser)(fragment, Space.WS(_))
  private lazy val multiLineParser = new ExpressionParser(nameParser)(fragment, ScalaWhitespace.whitespace)

  import Node._
  import nameParser._
  import stringParser.{string, plainString}

  // Parse a handful of top-level expressions before jumping into the deep end.
  def topLevelExpression[_: P]: P[TopLevelExprNode] = {
    P(`return` | variableDeclaration | assignment | expression)
  }

  private def `return`[_: P]: P[TopLevelExprNode] = P(Index ~~ "return" ~ expression ~~ Index).map(withPosition(TopLevelExprNode.ReturnNode))

  private def variableDeclaration[_: P]: P[TopLevelExprNode.VariableDeclarationNode] = {
    P(Index ~~ "let" ~~ Space.WS1 ~ ("mut" ~~ Space.WS1).!.?.map(_.isDefined) ~ name ~ typeParser.typing.? ~ "=" ~ expression ~~ Index)
      .map { case (startIndex, isMutable, name, tpe, value, endIndex) => (startIndex, name, isMutable, tpe, value, endIndex) }
      .map(withPosition(TopLevelExprNode.VariableDeclarationNode))
  }

  private def assignment[_: P]: P[TopLevelExprNode] = {
    P(Index ~~ address ~ StringIn("=", "+=", "-=", "*=", "/=").! ~ expression ~~ Index).map { case (startIndex, address, op, rhs, endIndex) =>
      val expression = op match {
        case "=" => rhs
        case augment =>
          val left = address
          val right = rhs
          augment match {
            case "+=" => ExprNode.AdditionNode(left, right, rhs.position)
            case "-=" => ExprNode.SubtractionNode(left, right, rhs.position)
            case "*=" => ExprNode.MultiplicationNode(left, right, rhs.position)
            case "/=" => ExprNode.DivisionNode(left, right, rhs.position)
          }
      }
      TopLevelExprNode.AssignmentNode(address, expression, Position(fragment, startIndex, endIndex))
    }
  }

  /**
    * An assignment address is either a plain variable or some property that is accessed on any accessible expression.
    *
    * Note that a member access such as `foo.bar` will be parsed as a variable due to how name paths are handled.
    */
  private def address[_: P]: P[ExprNode.AddressNode] = {
    // We can cast to PropertyAccessNode because we set minAccess to 1, which ensures that the parse results in such
    // a node.
    def prop = P(propertyAccess(accessible, minAccess = 1)).asInstanceOf[P[ExprNode.MemberAccessNode]]
    P(variable | prop)
  }

  def expression[_: P]: P[ExprNode] = P(ifElse | cond | whileLoop | forLoop | anonymousFunction | operatorExpression)
  def singleLineExpression[_: P]: P[ExprNode] = singleLineParser.expression

  private def ifElse[_: P]: P[ExprNode] = {
    def elsePart = P("else" ~~ Space.WS ~~ (implicitBlock | topLevelExpression))
    def thenStyle = P(expression ~ "then" ~ topLevelExpression ~ elsePart.?)

    def blockStyle = {
      P(singleLineExpression ~~ Space.terminators ~~ Index ~~ blockExpressions ~ "end" ~~ Index ~ elsePart.?).map {
        case (condition, startIndex, onTrueExpressions, endIndex, onFalse) =>
          val onTrue = withPosition(ExprNode.BlockNode)(startIndex, onTrueExpressions, endIndex)
          (condition, onTrue, onFalse)
      }
    }

    P(Index ~~ "if" ~~ Space.WS1 ~~ (thenStyle | blockStyle) ~~ Index)
      .map { case (startIndex, (condition, onTrue, onFalse), endIndex) => (startIndex, condition, onTrue, onFalse, endIndex) }
      .map(withPosition(ExprNode.IfElseNode))
  }

  private def cond[_: P]: P[ExprNode] = {
    def condCase = {
      P(Index ~~ operatorExpression ~ "=>" ~ topLevelExpression ~~ Index)
        .map(withPosition(ExprNode.CondCaseNode))
    }
    P(Index ~~ "cond" ~~ Space.terminators ~ condCase.repX(0, Space.terminators).map(_.toVector) ~ "end" ~~ Index)
      .map(withPosition(ExprNode.CondNode))
  }

  private def whileLoop[_: P]: P[ExprNode.WhileNode] = {
    def yieldStyle = P(expression ~ "yield" ~ topLevelExpression)
    def blockStyle = P(singleLineExpression ~~ implicitBlock)
    P(Index ~~ "while" ~~ Space.WS1 ~~ (yieldStyle | blockStyle) ~~ Index)
      .map { case (startIndex, (condition, body), endIndex) => (startIndex, condition, body, endIndex) }
      .map(withPosition(ExprNode.WhileNode))
  }

  private def forLoop[_: P]: P[ExprNode.ForNode] = {
    def yieldStyle = P(extractors ~ "yield" ~ topLevelExpression)
    def blockStyle = P(singleLineParser.extractors ~~ implicitBlock)
    P(Index ~~ "for" ~~ Space.WS1 ~~ (yieldStyle | blockStyle) ~~ Index)
      .map { case (startIndex, (extractors, body), endIndex) => (startIndex, extractors, body, endIndex) }
      .map(withPosition(ExprNode.ForNode))
  }

  private def extractors[_: P]: P[Vector[ExprNode.ExtractorNode]] = {
    // Because a `for` head may be terminated by a newline, we cannot allow trailing commas for extractors.
    def extractor = P(Index ~~ name ~ "<-" ~ expression ~~ Index).map(withPosition(ExprNode.ExtractorNode))
    P(extractor.rep(1, sep = ",")).map(_.toVector)
  }

  private def anonymousFunction[_: P]: P[ExprNode.AnonymousFunctionNode] = {
    P(Index ~~ anonymousFunctionParameters ~ "=>" ~ expression ~~ Index).map(withPosition(ExprNode.AnonymousFunctionNode))
  }

  private def anonymousFunctionParameters[_: P]: P[Vector[ExprNode.AnonymousFunctionParameterNode]] = {
    def simpleParameter = P(Index ~~ name ~~ Index).map {
      case (startIndex, name, endIndex) => withPosition(ExprNode.AnonymousFunctionParameterNode)(startIndex, name, None, endIndex)
    }
    def parameter = P(Index ~~ name ~ typeParser.typing.? ~~ Index).map(withPosition(ExprNode.AnonymousFunctionParameterNode))
    P(simpleParameter.map(Vector(_)) | "(" ~ parameter.rep(sep = ",").map(_.toVector) ~ ",".? ~ ")")
  }

  def operatorExpression[_: P]: P[ExprNode] = {
    import PrecedenceParser._
    PrecedenceParser.parser(
      operator = StringIn("||", "&&", "==", "!=", "<", "<=", ">", ">=", "|>", ":+", "+", "-", "*", "/"),
      operand = unary,
      operatorMeta = Map(
        "||" -> XaryOperator[ExprNode](1, ExprNode.DisjunctionNode),
        "&&" -> XaryOperator[ExprNode](2, ExprNode.ConjunctionNode),
        "==" -> BinaryOperator[ExprNode](3, ExprNode.EqualsNode),
        "!=" -> BinaryOperator[ExprNode](3, ExprNode.NotEqualsNode),
        "<" -> BinaryOperator[ExprNode](4, ExprNode.LessThanNode),
        "<=" -> BinaryOperator[ExprNode](4, ExprNode.LessThanEqualsNode),
        ">" -> BinaryOperator[ExprNode](4, ExprNode.greaterThan),
        ">=" -> BinaryOperator[ExprNode](4, ExprNode.greaterThanEquals),
        "|>" -> BinaryOperator[ExprNode](5, ExprNode.pipe),
        ":+" -> BinaryOperator[ExprNode](6, ExprNode.AppendNode),
        "+" -> BinaryOperator[ExprNode](7, ExprNode.AdditionNode),
        "-" -> BinaryOperator[ExprNode](7, ExprNode.SubtractionNode),
        "*" -> BinaryOperator[ExprNode](8, ExprNode.MultiplicationNode),
        "/" -> BinaryOperator[ExprNode](8, ExprNode.DivisionNode),
      ),
    )
  }

  // We apply NoCut here to allow the parser to backtrack if it doesn't find a multiplication/addition operator, while
  // still allowing cuts inside of unary applications and atoms.
  private def unary[_: P]: P[ExprNode] = NoCut(P(negation | logicalNot | atom))
  private def negation[_: P]: P[ExprNode] = P(Index ~~ "-" ~ atom ~~ Index).map { case (startIndex, expr, endIndex) =>
    // I don't want to put atom before negation, because I want the parser to handle the minus symbol before considering
    // atoms. However, the way it'd work with a naive implementation of this parser, a negative number would be parsed
    // as NegationNode(IntLiteralNode(x)). Hence we handle this specific case here.
    val position = Position(fragment, startIndex, endIndex)
    expr match {
      case ExprNode.IntLiteralNode(x, _) => ExprNode.IntLiteralNode(-x, position)
      case ExprNode.RealLiteralNode(x, _) => ExprNode.RealLiteralNode(-x, position)
      case _ => ExprNode.NegationNode(expr, position)
    }
  }
  private def logicalNot[_: P]: P[ExprNode] = P(Index ~~ "!" ~ atom ~~ Index).map(withPosition(ExprNode.LogicalNotNode))

  /**
    * Atomic operands of unary and binary operators.
    *
    * We parse property access here to reduce the amount of backtracking the parser needs to do. Even though all
    * dots are consumed greedily, we still create a chain of PropertyAccessNodes, because that'll make it easier to
    * to compile later.
    */
  private def atom[_: P]: P[ExprNode] = P(propertyAccess(accessible, minAccess = 0))

  /**
    * Surrounds an accessible expression with the possibility for property access. This is used by atom AND assignment
    * parsers to apply property access.
    *
    * @param minAccess The minimum number of times that the instance needs to be accessed.
    */
  private def propertyAccess[_: P](accessible: P[ExprNode], minAccess: Int): P[ExprNode] = {
    def propertyAccess = P((Index ~~ "." ~ name ~~ Index).rep(minAccess))
    P(accessible ~ propertyAccess).map { case (expr, propertyAccesses) =>
      // Create a PropertyAccessNode for every property access or just return the expression if there is
      // no property access.
      propertyAccesses.foldLeft(expr) { case (instance, (startIndex, name, endIndex)) =>
        ExprNode.MemberAccessNode(instance, name, Position(fragment, startIndex, endIndex))
      }
    }
  }

  /**
    * All expressions immediately accessible via postfix dot notation.
    */
  private def accessible[_: P]: P[ExprNode] = {
    P(literal | dynamicCall | simpleCall | call | objectMap | constructor | fixedFunction | variable | block | list | map | shape | symbol | enclosed)
  }

  /**
    * All expressions immediately accessible via postfix dot notation that can be used as call targets.
    */
  private def accessibleCallTarget[_: P]: P[ExprNode] = {
    P(literal | objectMap | constructor | fixedFunction | variable | block | list | map | shape | symbol | enclosed)
  }

  private def literal[_: P]: P[ExprNode] = {
    def real = P(Index ~~ LexicalParser.real ~~ Index).map(withPosition(ExprNode.RealLiteralNode))
    def int = P(Index ~~ LexicalParser.integer ~~ Index).map(withPosition(ExprNode.IntLiteralNode))
    def booleanLiteral = P(Index ~~ StringIn("true", "false").!.map(_.toBoolean) ~~ Index).map(withPosition(ExprNode.BoolLiteralNode))
    // Reals have to be parsed before ints so that ints don't consume the portion of the real before the fraction.
    P(real | int | booleanLiteral | string)
  }

  private def dynamicCall[_: P]: P[ExprNode] = {
    def prefix = P("dynamic" ~~ Space.WS ~~ singleTypeArgument)
    def arguments = P(("," ~ expression.rep(1, ",") ~ ",".?).?.map(_.getOrElse(Vector.empty)))
    P(Index ~~ prefix ~~ Space.WS ~~ "(" ~ plainString ~ arguments ~ ")" ~~ Index)
      .map { case (startIndex, resultType, name, arguments, endIndex) => (startIndex, name, resultType, arguments.toVector, endIndex) }
      .map(withPosition(ExprNode.DynamicCallNode))
  }

  private def constructor[_: P]: P[ExprNode] = P(Index ~~ namePath ~ Space.WS ~~ typeArguments ~~ Index).map(withPosition(ExprNode.ConstructorNode))

  private def simpleCall[_: P]: P[ExprNode] = P(Index ~~ namePath ~~ Space.WS ~~ arguments.rep(1) ~~ Index).map {
    case (startIndex, name, argumentLists, endIndex) =>
      val firstCall = withPosition(ExprNode.SimpleCallNode)(startIndex, name, argumentLists.head, endIndex)
      foldCalls(startIndex, firstCall, argumentLists.tail, endIndex)
  }

  /**
    * To avoid infinite left-recursion, we don't allow the target expression to be an atom, but rather either a
    * guaranteed property access, or an accessible itself without calls. To still allow calls such as `abc(a)(b)(c)`,
    * if `abc(a)` returns a callable function and so on, we are parsing all argument lists in a row.
    */
  private def call[_: P]: P[ExprNode] = {
    P(Index ~~ propertyAccess(accessibleCallTarget, minAccess = 0) ~~ Space.WS ~~ arguments.rep(1) ~~ Index).map((foldCalls _).tupled)
  }

  private def foldCalls(startIndex: Index, target: ExprNode, argumentLists: Seq[Vector[ExprNode]], endIndex: Index): ExprNode = {
    argumentLists.foldLeft(target) {
      case (target, arguments) => withPosition(ExprNode.CallNode)(startIndex, target, arguments, endIndex)
    }
  }

  private def arguments[_: P]: P[Vector[ExprNode]] = P("(" ~ expression.rep(sep = ",") ~ ",".? ~ ")").map(_.toVector)
  private def typeArguments[_: P]: P[Vector[TypeExprNode]] = P("[" ~ typeParser.typeExpression.rep(sep = ",") ~ ",".? ~ "]").map(_.toVector)
  private def singleTypeArgument[_: P]: P[TypeExprNode] = P("[" ~ typeParser.typeExpression ~ "]")

  private def fixedFunction[_: P]: P[ExprNode] = P(Index ~~ namePath ~ ".fixed" ~~ Space.WS ~~ typeArguments ~~ Index).map(withPosition(ExprNode.FixedFunctionNode))

  private def objectMap[_: P]: P[ExprNode.ObjectMapNode] = {
    def entry = P(Index ~~ name ~ "=" ~ expression ~~ Index).map(withPosition(ExprNode.ObjectEntryNode))
    def shorthand = P(Index ~~ name ~~ Index).map { case (startIndex, nameNode, endIndex) =>
      val position = Position(fragment, startIndex, endIndex)
      ExprNode.ObjectEntryNode(nameNode, ExprNode.VariableNode(NamePathNode(nameNode), position), position)
    }
    def entries = P((entry | shorthand).rep(sep = ",") ~ ",".?).map(_.toVector)
    P(Index ~~ namePath ~~ Space.WS ~~ typeArguments.? ~ "{" ~ entries ~ "}" ~~ Index).map(withPosition(ExprNode.ObjectMapNode))
  }

  private def variable[_: P]: P[ExprNode.VariableNode] = P(Index ~~ namePath ~~ Index).map(withPosition(ExprNode.VariableNode))

  def block[_: P]: P[ExprNode.BlockNode] = {
    P(Index ~~ "do" ~~ (Space.WS1 | Space.terminator) ~ blockExpressions ~ "end" ~~ Index).map(withPosition(ExprNode.BlockNode))
  }

  def implicitBlock[_: P]: P[ExprNode.BlockNode] = {
    P(Index ~~ Space.terminators ~~ blockExpressions ~ "end" ~~ Index).map(withPosition(ExprNode.BlockNode))
  }

  def blockExpressions[_: P]: P[Vector[TopLevelExprNode]] = {
    P(topLevelExpression.repX(0, Space.terminators).map(_.toVector))
  }

  private def list[_: P]: P[ExprNode] = {
    P(Index ~~ "[" ~ expression.rep(sep = ",").map(_.toVector) ~ ",".? ~ "]" ~~ Index).map(withPosition(ExprNode.ListNode))
  }

  private def map[_: P]: P[ExprNode] = {
    def keyValue = P(Index ~~ expression ~ "->" ~ expression ~~ Index).map(withPosition(ExprNode.KeyValueNode))
    P(Index ~~ "#[" ~ keyValue.rep(sep = ",").map(_.toVector) ~ ",".? ~ "]" ~~ Index).map(withPosition(ExprNode.MapNode))
  }

  private def shape[_: P]: P[ExprNode.ShapeValueNode] = {
    def property = P(Index ~~ name ~ ":" ~ expression ~~ Index).map(withPosition(ExprNode.ShapeValuePropertyNode))
    def shorthand = P(Index ~~ name ~~ Index).map { case (startIndex, nameNode, endIndex) =>
      val position = Position(fragment, startIndex, endIndex)
      ExprNode.ShapeValuePropertyNode(nameNode, ExprNode.VariableNode(NamePathNode(nameNode), position), position)
    }
    def properties = P((property | shorthand).rep(sep = ",") ~ ",".?).map(_.toVector)
    P(Index ~~ "%{" ~ properties ~ "}" ~~ Index).map(withPosition(ExprNode.ShapeValueNode))
  }

  private def symbol[_: P]: P[ExprNode.SymbolValueNode] = P(Index ~~ "#" ~ identifier ~~ Index).map(withPosition(ExprNode.SymbolValueNode))

  /**
    * Parses both enclosed expressions and tuples using the same parser. If the number of expressions is exactly one,
    * it's simply an enclosed expression. Otherwise, it is a tuple.
    *
    * Note that the inner expressions are always parsed with full whitespace, including newlines.
    */
  private def enclosed[_: P]: P[ExprNode] = {
    P(Index ~~ "(" ~ multiLineParser.expression.rep(sep = ",") ~ ",".? ~ ")" ~~ Index).map {
      case (_, Seq(expr), _) => expr
      case (startIndex, elements, endIndex) => ExprNode.TupleNode(elements.toVector, Position(fragment, startIndex, endIndex))
    }
  }
}
