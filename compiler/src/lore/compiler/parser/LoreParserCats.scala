package lore.compiler.parser

/*
import cats.parse.Parser.not
import cats.parse.{Accumulator, Parser, Parser0, Parser => P}
import cats.parse.Rfc5234.{alpha, digit, hexdig}
import lore.compiler.core.{Fragment, Position}
import lore.compiler.syntax.DeclNode.ModuleNode
import lore.compiler.syntax.ExprNode.{ConcatenationNode, StringLiteralNode, VariableNode}
import lore.compiler.syntax.Node.{Index, NameNode, NamePathNode, withPosition}
import lore.compiler.syntax.{DeclNode, ExprNode}

//val p: Parser[Char] = Parser.anyChar
//
//  p.parse("t")
//  // res0: Either[Error, Tuple2[String, Char]] = Right((,t))
//  p.parse("")
//  // res1: Either[Error, Tuple2[String, Char]] = Left(Error(0,NonEmptyList(InRange(0,,))))
//  p.parse("two")
//  // res2: Either[Error, Tuple2[String, Char]] = Right((wo,t))

// TODO (syntax): Move parsers that don't need `indentation` to other classes. In general, I suspect having to build
//                all these parsers each time indentation is changed is quite expensive. Maybe use lazy values or
//                `Parser.defer`.

class LoreParser(indentation: Int)(implicit fragment: Fragment) {
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Indentation handling.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  case class Rep[A, B](p1: Parser[A], min: Int, maxMinusOne: Int, acc1: Accumulator[A, B])
    extends Parser[B] {
    if (min < 1) throw new IllegalArgumentException(s"expected min >= 1, found: $min")

    private[this] val ignore: B = null.asInstanceOf[B]

    override def parseMut(state: State): B = {
      // first parse one, so we can initialize the appender with that value
      // then do the rest, with min -> min - 1 and
      // maxMinusOne -> maxMinusOne - 1 or Int.MaxValue as "forever" sentinel
      val head = p1.parseMut(state)

      def maxRemainingMinusOne =
        if (maxMinusOne == Int.MaxValue) Int.MaxValue else maxMinusOne - 1

      if (state.error ne null) ignore
      else if (state.capture) {
        val app = acc1.newAppender(head)
        if (repCapture(p1, min - 1, maxRemainingMinusOne, state, app)) app.finish()
        else ignore
      } else {
        repNoCapture(p1, min - 1, maxRemainingMinusOne, state)
        ignore
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Lexical.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  val underscore: P[Unit] = P.char('_')

  val identifierSpecialCharacter: P[Char] = P.charIn('?', '!')

  val identifier: P[String] =
    ((alpha | underscore) ~ (alpha | digit | underscore | identifierSpecialCharacter).rep).string
      .filter(!LoreSyntax.keywords.contains(_))

  val typePrefixCharacter: P[Unit] = P.char('+')

  val typeIdentifier: P[String] =
    ((alpha | underscore | typePrefixCharacter) ~ (alpha | digit | underscore | identifierSpecialCharacter)).string
      .filter(!LoreSyntax.keywords.contains(_))

  private def negatable[A](number: P[A])(implicit ev: Numeric[A]): P[A] = (P.charIn('+', '-').?.with1 ~ number).map {
    case (Some('-'), n) => ev.negate(n)
    case (_, n) => n
  }

  val number: P[Unit] = ((P.charIn('1' to '9') ~ digit.rep) | P.char('0')).void
  val fraction: P[Unit] = (P.char('.') ~ digit.rep(1)).void

  val integer: P[Long] = negatable(number.string.map(_.toLong))
  val real: P[Double] = negatable((number ~ fraction).string.map(_.toDouble))

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Names.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  private def genericNamePath(name: P[NameNode]): P[NamePathNode] =
    name.repSep(P.char('.')).map(_.iterator.toVector).map(NamePathNode(_))

  val name: P[NameNode] = (P.index.with1 ~ identifier ~ P.index).withPosition.map(NameNode.tupled)
  val namePath: P[NamePathNode] = genericNamePath(name)
  val typeName: P[NameNode] = (P.index.with1 ~ typeIdentifier ~ P.index).withPosition.map(NameNode.tupled)
  val typeNamePath: P[NamePathNode] = genericNamePath(typeName)
  val typeVariableName: P[NameNode] = (P.index.with1 ~ identifier ~ P.index).withPosition.map(NameNode.tupled)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Strings.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * `chars` quickly eats large chunks of string characters conservatively until a potential escape sequence is
    * encountered.
    */
  val stringChars: P[String] = P.charsWhile(c => c != '\n' && c != '\'' && c != '\\' && c != '$').string

  val stringEscape: P[String] = {
    val basicEscape = P.charIn('n', 'r', 't', '\'', '$', '\\').map {
      case 'n' => '\n'
      case 'r' => '\r'
      case 't' => '\t'
      // ' $ \
      case x => x
    }

    val unicodeEscape = P.char('u') *> hexdig.rep(4, 4).string.map {
      string => Integer.parseInt(string, 16).toChar.toString
    }

    P.char('\\') *> (basicEscape.map(_.toString).backtrack | unicodeEscape)
  }

  val stringContent: P[StringLiteralNode] =
    (P.index.with1 ~ (stringChars | stringEscape).rep(1) ~ P.index)
      .map { case ((startIndex, strings), endIndex) => (startIndex, strings.foldLeft("")(_ + _), endIndex) }
      .withPosition
      .map(StringLiteralNode.tupled)

  /**
    * This parser attempts to shove as many characters into StringLiteralNode as possible.
    */
  val notStringEnd: P[StringLiteralNode] =
    (P.index.with1 ~ (not(P.charIn('\n', '\'')).with1 ~ P.anyChar).string ~ P.index)
      .withPosition
      .map(StringLiteralNode.tupled)

  val stringInterpolation: P[ExprNode] = {
    val simple = (P.index.with1 ~ name ~ P.index)
      .map { case ((startIndex, nameNode), endIndex) => (startIndex, NamePathNode(nameNode), endIndex) }
      .withPosition
      .map(VariableNode.tupled)
    val expression: P[ExprNode] = ???
    val block = P.char('{') *> expression <* P.char('}')

    P.char('$') *> (block.backtrack | simple)
  }

  val string: P[ExprNode] = {
    // We have to check `stringContent`, `stringInterpolation`, and `notStringEnd` exactly in this order, otherwise
    // `notStringEnd` would consume parts that are meant to be escapes or interpolations.
    stringBase(stringContent.backtrack | stringInterpolation.backtrack | notStringEnd)
  }

  /**
    * A plain string that does not contain interpolation.
    */
  val plainString: P[StringLiteralNode] = {
    stringBase(stringContent.backtrack | notStringEnd)
      // Due to the nature of the parser, the result must be a StringLiteralNode, so the cast here is fine.
      .map(_.asInstanceOf[StringLiteralNode])
  }

  private def stringBase(part: P[ExprNode]): P[ExprNode] = {
    (P.index.with1 ~ part.rep.map(_.iterator.toVector).surroundedBy(P.char('\'')) ~ P.index).map {
      case ((startIndex, Vector()), endIndex) => StringLiteralNode("", Position(fragment, startIndex, endIndex))
      // This can either be a single string literal or any expression enclosed as such: '$expr'.
      case ((startIndex, Vector(expression)), endIndex) => expression match {
        case literal: StringLiteralNode =>
          // One unfortunate side effect of immutable positions is that we can't easily change the position of an
          // expression. The 'content' parser parses a string without the enclosing '' being factored in for the index.
          // So a string 'abc' at the beginning of a file would start at index 1 instead of 0. We reconstruct this for
          // string literals. Expressions aren't as easily changed with a new position, and as such we accept that an
          // expression such as '$value' starts at index 2, after the $.
          StringLiteralNode(literal.value, Position(fragment, startIndex, endIndex))
        case _ => expression
      }
      case ((startIndex, strings), endIndex) => ConcatenationNode(strings, Position(fragment, startIndex, endIndex))
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Position management.
  // TODO (syntax): Move into a utility object.
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  implicit class IndexParserExtension1[T1](parser: P[((Index, T1), Index)])(implicit fragment: Fragment) {
    def withPosition: P[(T1, Position)] = parser.map {
      case ((startIndex, p1), endIndex) => (p1, Position(fragment, startIndex, endIndex))
    }
  }

  implicit class IndexParserExtension1Untupled[T1](parser: P[(Index, T1, Index)])(implicit fragment: Fragment) {
    // TODO (syntax): Rename?
    def withPosition: P[(T1, Position)] = parser.map {
      case (startIndex, p1, endIndex) => (p1, Position(fragment, startIndex, endIndex))
    }
  }

//   TODO (syntax): There HAS to be a way to implement this handling of `Parser` and `Parser0` with less boilerplate
//                  and `asInstanceOf`.
//  implicit class IndexParserExtension1[Px[_] <: Parser0[_], T1](parser: Px[((Index, T1), Index)])(implicit fragment: Fragment) {
//    def withPosition: Px[(T1, Position)] = parser.map {
//      case ((startIndex, p1), endIndex) => (p1, Position(fragment, startIndex, endIndex))
//    }.asInstanceOf[Px[(T1, Position)]]
//  }
//
//  implicit class IndexParserExtension1Untupled[Px[_] <: Parser0[_], T1](parser: Px[(Index, T1, Index)])(implicit fragment: Fragment) {
//    // TODO (syntax): Rename?
//    def withPosition: Px[(T1, Position)] = parser.map {
//      case (startIndex, p1, endIndex) => (p1, Position(fragment, startIndex, endIndex))
//    }.asInstanceOf[Px[(T1, Position)]]
//  }
}
*/