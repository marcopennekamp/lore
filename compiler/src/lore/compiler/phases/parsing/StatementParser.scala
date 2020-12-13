package lore.compiler.phases.parsing

import fastparse.ScalaWhitespace._
import fastparse._
import lore.compiler.syntax._
import lore.compiler.core.{Fragment, Position}
import lore.compiler.syntax.ExprNode.StringLiteralNode

class StatementParser(typeParser: TypeParser)(implicit fragment: Fragment) {
  import Node._
  import LexicalParser.{identifier, hexDigit}

  // There is only one "true" statement: return.
  def statement[_: P]: P[StmtNode] = P(returnStatement | topLevelExpression)
  private def returnStatement[_: P]: P[StmtNode] = P(Index ~ "return" ~ expression).map(withIndex(StmtNode.ReturnNode))

  // Parse a handful of top-level expressions before jumping into the deep end.
  def topLevelExpression[_: P]: P[TopLevelExprNode] = {
    P(variableDeclaration | assignment | expression)
  }

  private def variableDeclaration[_: P]: P[TopLevelExprNode.VariableDeclarationNode] = {
    P(Index ~ "let" ~ "mut".!.?.map(_.isDefined) ~ identifier ~ typeParser.typing.? ~ "=" ~ expression)
      .map { case (index, isMutable, name, tpe, value) => (index, name, isMutable, tpe, value) }
      .map(withIndex(TopLevelExprNode.VariableDeclarationNode))
  }

  private def assignment[_: P]: P[TopLevelExprNode] = {
    P(Index ~ address ~ StringIn("=", "+=", "-=", "*=", "/=").! ~ expression).map { case (index, address, op, rhs) =>
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
      TopLevelExprNode.AssignmentNode(address, expression, Position(fragment, index))
    }
  }

  /**
    * An assignment address is either a plain variable or some property that is accessed on any accessible expression.
    */
  private def address[_: P]: P[ExprNode.AddressNode] = {
    // We can cast to PropertyAccessNode because we set minAccess to 1, which ensures that the parse results in such
    // a node.
    def prop = P(propertyAccess(minAccess = 1)).asInstanceOf[P[ExprNode.MemberAccessNode]]
    P(prop | variable)
  }

  def expression[_: P]: P[ExprNode] = P(ifElse | whileLoop | forLoop | operatorExpression)

  private def ifElse[_: P]: P[ExprNode] = {
    P(Index ~ "if" ~ "(" ~ expression ~ ")" ~ statement ~ ("else" ~ statement).?)
      .map { case (index, condition, onTrue, onFalse) => (index, condition, onTrue, onFalse.getOrElse(ExprNode.UnitNode(Position(fragment, index)))) }
      .map(withIndex(ExprNode.IfElseNode))
  }

  private def whileLoop[_: P]: P[ExprNode.WhileNode] = {
    P(Index ~ "while" ~ "(" ~ expression ~ ")" ~ statement).map(withIndex(ExprNode.WhileNode))
  }

  private def forLoop[_: P]: P[ExprNode.ForNode] = {
    def extractor = P(Index ~ identifier ~ "<-" ~ expression).map(withIndex(ExprNode.ExtractorNode))
    P(Index ~ "for" ~ "(" ~ extractor.rep(1, sep = ",") ~ ")" ~ statement)
      .map { case (index, extractors, stat) => (index, extractors.toVector, stat) }
      .map(withIndex(ExprNode.ForNode))
  }

  // TODO: The single & and | style feels quite weird when actually using it. Maybe we should just introduce
  //       operators && and || or "and" and "or". Similarly with the not operator. And I don't quite like =/=
  //       either, in hindsight.
  def operatorExpression[_: P]: P[ExprNode] = {
    import PrecedenceParser._
    PrecedenceParser.parser(
      operator = StringIn("|", "&", "==", "=/=", "<", "<=", ">", ">=", ":+", "+", "-", "*", "/"),
      operand = unary,
      operatorMeta = Map(
        "|" -> XaryOperator[ExprNode](1, ExprNode.DisjunctionNode),
        "&" -> XaryOperator[ExprNode](2, ExprNode.ConjunctionNode),
        "==" -> BinaryOperator[ExprNode](3, ExprNode.EqualsNode),
        "=/=" -> BinaryOperator[ExprNode](3, ExprNode.NotEqualsNode),
        "<" -> BinaryOperator[ExprNode](4, ExprNode.LessThanNode),
        "<=" -> BinaryOperator[ExprNode](4, ExprNode.LessThanEqualsNode),
        ">" -> BinaryOperator[ExprNode](4, ExprNode.GreaterThanNode),
        ">=" -> BinaryOperator[ExprNode](4, ExprNode.GreaterThanEqualsNode),
        ":+" -> BinaryOperator[ExprNode](5, ExprNode.AppendNode),
        "+" -> BinaryOperator[ExprNode](6, ExprNode.AdditionNode),
        "-" -> BinaryOperator[ExprNode](6, ExprNode.SubtractionNode),
        "*" -> BinaryOperator[ExprNode](7, ExprNode.MultiplicationNode),
        "/" -> BinaryOperator[ExprNode](7, ExprNode.DivisionNode),
      ),
    )
  }

  // We apply NoCut here to allow the parser to backtrack if it doesn't find a multiplication/addition operator, while
  // still allowing cuts inside of unary applications and atoms.
  private def unary[_: P]: P[ExprNode] = NoCut(P(negation | logicalNot | atom))
  private def negation[_: P]: P[ExprNode] = P(Index ~ "-" ~ atom).map { case (index, expr) =>
    // I don't want to put atom before negation, because I want the parser to handle the minus symbol before considering
    // atoms. However, the way it'd work with a naive implementation of this parser, a negative number would be parsed
    // as NegationNode(IntLiteralNode(x)). Hence we handle this specific case here.
    // TODO: We could also handle this inefficiency during the transformation phase...
    val position = Position(fragment, index)
    expr match {
      case ExprNode.IntLiteralNode(x, _) => ExprNode.IntLiteralNode(-x, position)
      case ExprNode.RealLiteralNode(x, _) => ExprNode.RealLiteralNode(-x, position)
      case _ => ExprNode.NegationNode(expr, position)
    }
  }
  private def logicalNot[_: P]: P[ExprNode] = P(Index ~ "~" ~ atom).map(withIndex(ExprNode.LogicalNotNode))

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
        ExprNode.MemberAccessNode(instance, name, Position(fragment, index))
      }
    }
  }

  /**
    * All expressions immediately accessible via postfix dot notation.
    */
  private def accessible[_: P]: P[ExprNode] = {
    P(literal | fixedCall | dynamicCall | call | objectMap | variable | block | list | map | enclosed)
  }

  private def literal[_: P]: P[ExprNode] = {
    def real = P(Index ~ LexicalParser.real).map(withIndex(ExprNode.RealLiteralNode))
    def int = P(Index ~ LexicalParser.integer).map(withIndex(ExprNode.IntLiteralNode))
    def booleanLiteral = P(Index ~ StringIn("true", "false").!.map(_.toBoolean)).map(withIndex(ExprNode.BoolLiteralNode))
    // Reals have to be parsed before ints so that ints don't consume the portion of the real before the fraction.
    P(real | int | booleanLiteral | string)
  }

  private def fixedCall[_: P]: P[ExprNode] = P(Index ~ identifier ~ ".fixed" ~~ Space.WS ~~ typeArguments ~~ Space.WS ~~ arguments).map(withIndex(ExprNode.FixedFunctionCallNode))

  private def dynamicCall[_: P]: P[ExprNode] = P(Index ~ "dynamic" ~~ Space.WS ~~ singleTypeArgument ~~ Space.WS ~~ arguments).map(withIndex(ExprNode.DynamicCallNode))

  private def call[_: P]: P[ExprNode] = P(Index ~ identifier ~~ Space.WS ~~ arguments).map(withIndex(ExprNode.SimpleCallNode))

  private def arguments[_: P]: P[Vector[ExprNode]] = P("(" ~ expression.rep(sep = ",") ~ ")").map(_.toVector)
  private def typeArguments[_: P]: P[Vector[TypeExprNode]] = P("[" ~ typeParser.typeExpression.rep(sep = ",") ~ "]").map(_.toVector)
  private def singleTypeArgument[_: P]: P[TypeExprNode] = P("[" ~ typeParser.typeExpression ~ "]")

  private def objectMap[_: P]: P[ExprNode.ObjectMapNode] = {
    def entry = P(Index ~ identifier ~ "=" ~ expression).map(withIndex(ExprNode.ObjectEntryNode))
    def shorthand = P(Index ~ identifier).map { case (index, name) =>
      val position = Position(fragment, index)
      ExprNode.ObjectEntryNode(name, ExprNode.VariableNode(name, position), position)
    }
    def entries = P((entry | shorthand).rep(sep = ",")).map(_.toVector)
    P(Index ~ identifier ~ "{" ~ entries ~ "}").map(withIndex(ExprNode.ObjectMapNode))
  }

  private def variable[_: P]: P[ExprNode.VariableNode] = P(Index ~ identifier).map(withIndex(ExprNode.VariableNode))

  def block[_: P]: P[ExprNode.BlockNode] = {
    def statements = P(statement.repX(0, Space.terminators).map(_.toVector))
    P(Index ~ "{" ~ statements ~ "}").map(withIndex(ExprNode.BlockNode))
  }

  private def list[_: P]: P[ExprNode] = {
    P(Index ~ "[" ~ expression.rep(sep = ",").map(_.toVector) ~ "]").map(withIndex(ExprNode.ListNode))
  }

  private def map[_: P]: P[ExprNode] = {
    def keyValue = P(Index ~ expression ~ "->" ~ expression).map(withIndex(ExprNode.KeyValueNode))
    P(Index ~ "%{" ~ keyValue.rep(sep = ",").map(_.toVector) ~ "}").map(withIndex(ExprNode.MapNode))
  }

  /**
    * Parses both enclosed expressions and tuples using the same parser. If the number of expressions is exactly one,
    * it's simply an enclosed expression. Otherwise, it is a tuple.
    */
  private def enclosed[_: P]: P[ExprNode] = {
    P(Index ~ "(" ~ (expression ~ ("," ~ expression).rep).? ~ ")").map {
      case (index, None) => ExprNode.UnitNode(Position(fragment, index))
      case (_, Some((expr, Seq()))) => expr
      case (index, Some((left, expressions))) => ExprNode.TupleNode(left +: expressions.toVector, Position(fragment, index))
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Strings and interpolation! (Thanks again to Li Haoyi and his scalaparse example, which I used as a base for this.)
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def string[_: P]: P[ExprNode] = {
    // Strings are sensitive to whitespace.
    import fastparse.NoWhitespace._

    // Helper to quickly gobble up large chunks of uninteresting characters. We break out conservatively, even if
    // we don't know it's a "real" escape sequence: worst come to worst it turns out to be a dud and we go back into
    // a CharsChunk next rep.
    def stringChars = P(CharsWhile(c => c != '\n' && c != '\'' && c != '\\' && c != '$').!)
    // The repetitions here attempt to shove as many characters into StringLiteralNode as possible.
    def notStringEnd = P(Index ~ (!CharIn("\n'") ~ AnyChar).!).map(withIndex(ExprNode.StringLiteralNode))
    def content = P(Index ~ (stringChars | escape).rep(1))
      .map { case (index, strings) => (index, strings.foldLeft("")(_ + _)) }
      .map(withIndex(ExprNode.StringLiteralNode))
    // We have to check content, interpolation, notStringEnd exactly in this order, otherwise notStringEnd would
    // consume parts that are meant to be escapes or interpolations.
    P(Index ~ "'" ~ (content | interpolation | notStringEnd).rep.map(_.toVector) ~ "'")
      .map {
        case (index, Vector()) => ExprNode.StringLiteralNode("", Position(fragment, index))
        // This can either be a single string literal or any expression enclosed as such: '$expr'.
        case (index, Vector(expression)) => expression match {
          case literal: StringLiteralNode =>
            // One unfortunate side effect of immutable positions is that we can't easily change the position of
            // an expression. The 'content' parser parses a string without the enclosing '' being factored in for
            // the index. So a string 'abc' at the beginning of a file would start at index 1 instead of 0. We
            // reconstruct this for string literals. Expressions aren't as easily changed with a new position, and
            // as such we accept that an expression such as '$value' starts at index 2, after the $.
            StringLiteralNode(literal.value, Position(fragment, index))
          case _ => expression
        }
        case (index, strings) => ExprNode.ConcatenationNode(strings, Position(fragment, index))
      }
  }

  private def interpolation[_: P]: P[ExprNode] = {
    // Strings are sensitive to whitespace.
    import fastparse.NoWhitespace._

    def simple = P(Index ~ identifier).map(withIndex(ExprNode.VariableNode))
    def block = P("{" ~ NoCut(expression) ~ "}")
    P("$" ~ (block | simple))
  }

  private def escape[_: P]: P[String] = {
    // Strings are sensitive to whitespace.
    import fastparse.NoWhitespace._

    def unicodeEscape = P("u" ~ (hexDigit ~ hexDigit ~ hexDigit ~ hexDigit).!).map {
      string => Integer.parseInt(string, 16).toChar.toString
    }
    def basicEscape = P(CharIn("""nrt'$\\""").!).map {
      case "n" => "\n"
      case "r" => "\r"
      case "t" => "\t"
      // ' $ \
      case x => x
    }
    P("\\" ~ (basicEscape | unicodeEscape))
  }
}
