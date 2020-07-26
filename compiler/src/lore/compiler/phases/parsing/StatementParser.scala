package lore.compiler.phases.parsing

import fastparse.ScalaWhitespace._
import fastparse._
import lore.compiler.syntax._
import lore.compiler.core.Fragment

class StatementParser(typeParser: TypeParser)(implicit fragment: Fragment) {
  import Node._
  import LexicalParser.{identifier, hexDigit}

  // There is only one "true" statement: return.
  def statement[_: P]: P[StmtNode] = P(returnStatement | topLevelExpression)
  private def returnStatement[_: P]: P[StmtNode] = P(Index ~ "return" ~ expression).map(withIndex(StmtNode.ReturnNode(_)))

  // Parse a handful of top-level expressions before jumping into the deep end.
  def topLevelExpression[_: P]: P[TopLevelExprNode] = {
    P(variableDeclaration | assignment | continuation | expression)
  }

  private def variableDeclaration[_: P]: P[TopLevelExprNode.VariableDeclarationNode] = {
    P(Index ~ ("const" | "let").! ~ identifier ~ typeParser.typing.? ~ "=" ~ expression)
      .map { case (index, qualifier, name, tpe, value) => (index, name, qualifier == "let", tpe, value) }
      .map(withIndex(TopLevelExprNode.VariableDeclarationNode(_, _, _, _)))
  }

  private def assignment[_: P]: P[TopLevelExprNode] = {
    P(Index ~ address ~ StringIn("=", "+=", "-=", "*=", "/=").! ~ expression).map { case (index, address, op, rhs) =>
      val expression = op match {
        case "=" => rhs
        case augment =>
          val left = address
          val right = rhs
          val node = augment match {
            case "+=" => ExprNode.AdditionNode(left, right)
            case "-=" => ExprNode.SubtractionNode(left, right)
            case "*=" => ExprNode.MultiplicationNode(left, right)
            case "/=" => ExprNode.DivisionNode(left, right)
          }
          attachIndex(node)(rhs.position.index)
      }
      attachIndex(TopLevelExprNode.AssignmentNode(address, expression))(index)
    }
  }

  /**
    * An assignment address is either a plain variable or some property that is accessed on any accessible expression.
    */
  private def address[_: P]: P[ExprNode.AddressNode] = {
    // We can cast to PropertyAccessNode because we set minAccess to 1, which ensures that the parse results in such
    // a node.
    def prop = P(propertyAccess(minAccess = 1)).asInstanceOf[P[ExprNode.PropertyAccessNode]]
    P(prop | variable)
  }

  private def continuation[_: P] = {
    def constructorCall = {
      P(Index ~ ("." ~ identifier).? ~ arguments).map(withIndex(TopLevelExprNode.ConstructorCallNode(_, _)))
    }
    def thisCall = P("this" ~ constructorCall)
    def superCall = P("super" ~ constructorCall)
    def constructCall = {
      P(Index ~ "construct" ~ arguments ~ ("with" ~ superCall).?).map(withIndex(TopLevelExprNode.ConstructNode(_, _)))
    }
    P(thisCall | constructCall)
  }

  // Parse expressions. Finally!
  def expression[_: P]: P[ExprNode] = P(ifElse | repetition | iteration | operatorExpression)

  private def ifElse[_: P]: P[ExprNode] = {
    P(Index ~ "if" ~ "(" ~ expression ~ ")" ~ statement ~ ("else" ~ statement).?)
      .map { case (index, condition, onTrue, onFalse) => (index, condition, onTrue, onFalse.getOrElse(ExprNode.UnitNode())) }
      .map(withIndex(ExprNode.IfElseNode(_, _, _)))
  }

  private def repetition[_: P]: P[ExprNode.RepetitionNode] = {
    P(Index ~ "while" ~ "(" ~ expression ~ ")" ~ statement).map(withIndex(ExprNode.RepetitionNode(_, _)))
  }

  private def iteration[_: P]: P[ExprNode.IterationNode] = {
    def extractor = P(Index ~ identifier ~ "<-" ~ expression).map(withIndex(ExprNode.ExtractorNode(_, _)))
    P(Index ~ "for" ~ "(" ~ extractor.rep(1, sep = ",") ~ ")" ~ statement)
      .map { case (index, extractors, stat) => (index, extractors.toList, stat) }
      .map(withIndex(ExprNode.IterationNode(_, _)))
  }

  // TODO: The single & and | style feels quite weird when actually using it. Maybe we should just introduce
  //       operators && and || or "and" and "or". Similarly with the not operator. And I don't quite like =/=
  //       either, in hindsight.
  def operatorExpression[_: P]: P[ExprNode] = {
    import PrecedenceParser._
    PrecedenceParser.parser(
      operator = StringIn("|", "&", "==", "=/=", "<", "<=", ">", ">=", "+", "-", "*", "/"),
      operand = unary,
      operatorMeta = Map(
        "|" -> XaryOperator[ExprNode](1, ExprNode.DisjunctionNode(_)),
        "&" -> XaryOperator[ExprNode](2, ExprNode.ConjunctionNode(_)),
        "==" -> BinaryOperator[ExprNode](3, ExprNode.EqualsNode(_, _)),
        "=/=" -> BinaryOperator[ExprNode](3, ExprNode.NotEqualsNode(_, _)),
        "<" -> BinaryOperator[ExprNode](4, ExprNode.LessThanNode(_, _)),
        "<=" -> BinaryOperator[ExprNode](4, ExprNode.LessThanEqualsNode(_, _)),
        ">" -> BinaryOperator[ExprNode](4, ExprNode.GreaterThanNode(_, _)),
        ">=" -> BinaryOperator[ExprNode](4, ExprNode.GreaterThanEqualsNode(_, _)),
        "+" -> BinaryOperator[ExprNode](5, ExprNode.AdditionNode(_, _)),
        "-" -> BinaryOperator[ExprNode](5, ExprNode.SubtractionNode(_, _)),
        "*" -> BinaryOperator[ExprNode](6, ExprNode.MultiplicationNode(_, _)),
        "/" -> BinaryOperator[ExprNode](6, ExprNode.DivisionNode(_, _)),
      ),
    )
  }

  // We apply NoCut here to allow the parser to backtrack if it doesn't find a multiplication/addition operator, while
  // still allowing cuts inside of unary applications and atoms.
  private def unary[_: P]: P[ExprNode] = NoCut(P(negation | logicalNot | atom))
  private def negation[_: P]: P[ExprNode] = P(Index ~ "-" ~ atom).map {
    // I don't want to put atom before negation, because I want the parser to handle the minus symbol before considering
    // atoms. However, the way it'd work with a naive implementation of this parser, a negative number would be parsed
    // as NegationNode(IntLiteralNode(x)). Hence we handle this specific case here.
    case (index, ExprNode.IntLiteralNode(x, state)) => withIndex(ExprNode.IntLiteralNode(_, state))(index, -x)
    case (index, ExprNode.RealLiteralNode(x, state)) => withIndex(ExprNode.RealLiteralNode(_, state))(index, -x)
    case (index, expr) => attachIndex(ExprNode.NegationNode(expr))(index)
  }
  private def logicalNot[_: P]: P[ExprNode] = P(Index ~ "~" ~ atom).map(withIndex(ExprNode.LogicalNotNode(_)))

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
        attachIndex(ExprNode.PropertyAccessNode(instance, name))(index)
      }
    }
  }

  /**
    * All expressions immediately accessible via postfix dot notation.
    */
  private def accessible[_: P]: P[ExprNode] = {
    P(literal | fixedCall | dynamicCall | call | variable | block | list | map | enclosed)
  }

  private def literal[_: P]: P[ExprNode] = {
    def real = P(LexicalParser.real).map(ExprNode.RealLiteralNode(_))
    def int = P(LexicalParser.integer).map(ExprNode.IntLiteralNode(_))
    def booleanLiteral = P(StringIn("true", "false").!).map(_.toBoolean).map(ExprNode.BoolLiteralNode(_))
    // Reals have to be parsed before ints so that ints don't consume the portion of the real before the fraction.
    P(Index ~ (real | int | booleanLiteral | string)).map(withIndex(identity _))
  }
  private def fixedCall[_: P]: P[ExprNode] = P(Index ~ identifier ~ ".fixed" ~ typeArguments ~ arguments).map(withIndex(ExprNode.FixedFunctionCallNode(_, _, _)))
  private def dynamicCall[_: P]: P[ExprNode] = P(Index ~ "dynamic" ~ singleTypeArgument ~ arguments).map(withIndex(ExprNode.DynamicCallNode(_, _)))
  private def call[_: P]: P[ExprNode] = P(Index ~ identifier ~ ("." ~ identifier).? ~ arguments).map(withIndex(ExprNode.SimpleCallNode(_, _, _)))
  def arguments[_: P]: P[List[ExprNode]] = P("(" ~ expression.rep(sep = ",") ~ ")").map(_.toList)
  private def typeArguments[_: P]: P[List[TypeExprNode]] = P("[" ~ typeParser.typeExpression.rep(sep = ",") ~ "]").map(_.toList)
  private def singleTypeArgument[_: P]: P[TypeExprNode] = P("[" ~ typeParser.typeExpression ~ "]")
  private def variable[_: P]: P[ExprNode.VariableNode] = P(Index ~ identifier).map(withIndex(ExprNode.VariableNode(_)))
  def block[_: P]: P[ExprNode.BlockNode] = {
    def statements = P(statement.repX(0, Space.terminators).map(_.toList))
    P(Index ~ "{" ~ statements ~ "}").map(withIndex(ExprNode.BlockNode(_)))
  }
  private def list[_: P]: P[ExprNode] = {
    P(Index ~ "[" ~ expression.rep(sep = ",") ~ "]")
      .map { case (index, expressions) => (index, expressions.toList) }
      .map(withIndex(ExprNode.ListNode(_)))
  }
  private def map[_: P]: P[ExprNode] = {
    def keyValue = P(Index ~ expression ~ "->" ~ expression).map(withIndex(ExprNode.KeyValueNode(_, _)))
    P(Index ~ "%{" ~ keyValue.rep(sep = ",") ~ "}")
      .map { case (index, kvs) => (index, kvs.toList) }
      .map(withIndex(ExprNode.MapNode(_)))
  }

  /**
    * Parses both enclosed expressions and tuples using the same parser. If the number of expressions is exactly one,
    * it's simply an enclosed expression. Otherwise, it is a tuple.
    */
  private def enclosed[_: P]: P[ExprNode] = {
    P(Index ~ "(" ~ (expression ~ ("," ~ expression).rep).? ~ ")").map {
      case (index, None) => attachIndex(ExprNode.UnitNode())(index)
      case (_, Some((expr, Seq()))) => expr
      case (index, Some((left, expressions))) => attachIndex(ExprNode.TupleNode(left +: expressions.toList))(index)
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
    def notStringEnd = P(Index ~ (!CharIn("\n'") ~ AnyChar).!).map(withIndex(ExprNode.StringLiteralNode(_)))
    def content = P(Index ~ (stringChars | escape).rep(1))
      .map { case (index, strings) => (index, strings.foldLeft("")(_ + _)) }
      .map(withIndex(ExprNode.StringLiteralNode(_)))
    // We have to check content, interpolation, notStringEnd exactly in this order, otherwise notStringEnd would
    // consume parts that are meant to be escapes or interpolations.
    P(Index ~ "'" ~ (content | interpolation | notStringEnd).rep ~ "'")
      .map { case (index, nodes) => (index, nodes.toList) }
      .map {
        case (index, List()) => attachIndex(ExprNode.StringLiteralNode(""))(index)
        // This can either be a single string literal or any expression enclosed as such: '$expr'.
        case (_, List(expression)) => expression
        case (index, strings) => attachIndex(ExprNode.ConcatenationNode(strings))(index)
      }
  }

  private def interpolation[_: P]: P[ExprNode] = {
    // Strings are sensitive to whitespace.
    import fastparse.NoWhitespace._

    def simple = P(Index ~ identifier).map(withIndex(ExprNode.VariableNode(_)))
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
