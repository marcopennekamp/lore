package lore.compiler.parsing

import fastparse._
import lore.compiler.core.{Fragment, Position}
import lore.compiler.parsing.LexicalParser.hexDigit
import lore.compiler.syntax.ExprNode
import lore.compiler.syntax.ExprNode.StringLiteralNode
import lore.compiler.syntax.Node.{NamePathNode, withPosition}

/**
  * Thanks again to Li Haoyi and his scalaparse example, which I used as a base for this.
  */
class StringParser(nameParser: NameParser, expressionParser: ExpressionParser)(implicit fragment: Fragment) {
  // Strings are sensitive to whitespace.
  import expressionParser.expression
  import fastparse.NoWhitespace._
  import nameParser.name

  def string[_: P]: P[ExprNode] = {
    // We have to check content, interpolation, notStringEnd exactly in this order, otherwise notStringEnd would
    // consume parts that are meant to be escapes or interpolations.
    P(stringBase(P(content | interpolation | notStringEnd)))
  }

  /**
    * A plain string that does not contain interpolation.
    */
  def plainString[_: P]: P[StringLiteralNode] = {
    P(stringBase(P(content | notStringEnd)))
      // Due to the nature of the parser, the result must be a StringLiteralNode, so the cast here is fine.
      .map(_.asInstanceOf[StringLiteralNode])
  }

  private def stringBase[_: P](part: => P[ExprNode]): P[ExprNode] = {
    P(Index ~~ "'" ~ part.rep.map(_.toVector) ~ "'" ~~ Index).map {
      case (startIndex, Vector(), endIndex) => ExprNode.StringLiteralNode("", Position(fragment, startIndex, endIndex))
      // This can either be a single string literal or any expression enclosed as such: '$expr'.
      case (startIndex, Vector(expression), endIndex) => expression match {
        case literal: StringLiteralNode =>
          // One unfortunate side effect of immutable positions is that we can't easily change the position of
          // an expression. The 'content' parser parses a string without the enclosing '' being factored in for
          // the index. So a string 'abc' at the beginning of a file would start at index 1 instead of 0. We
          // reconstruct this for string literals. Expressions aren't as easily changed with a new position, and
          // as such we accept that an expression such as '$value' starts at index 2, after the $.
          StringLiteralNode(literal.value, Position(fragment, startIndex, endIndex))
        case _ => expression
      }
      case (startIndex, strings, endIndex) => ExprNode.ConcatenationNode(strings, Position(fragment, startIndex, endIndex))
    }
  }

  /**
    * Helper to quickly gobble up large chunks of uninteresting characters. We break out conservatively, even if we
    * don't know it's a "real" escape sequence: worst come to worst it turns out to be a dud and we go back into a
    * CharsChunk next rep.
    */
  private def chars[_: P]: P[String] = P(CharsWhile(c => c != '\n' && c != '\'' && c != '\\' && c != '$').!)

  /**
    * This parser attempts to shove as many characters into StringLiteralNode as possible.
    */
  private def notStringEnd[_: P]: P[StringLiteralNode] = P(Index ~~ (!CharIn("\n'") ~ AnyChar).! ~~ Index).map(withPosition(ExprNode.StringLiteralNode))

  private def content[_: P]: P[StringLiteralNode] = P(Index ~~ (chars | escape).rep(1) ~~ Index)
    .map { case (startIndex, strings, endIndex) => (startIndex, strings.foldLeft("")(_ + _), endIndex) }
    .map(withPosition(ExprNode.StringLiteralNode))

  private def interpolation[_: P]: P[ExprNode] = {
    // Strings are sensitive to whitespace.
    import fastparse.NoWhitespace._

    def simple = P(Index ~~ name ~~ Index)
      .map { case (startIndex, nameNode, endIndex) => (startIndex, NamePathNode(nameNode), endIndex) }
      .map(withPosition(ExprNode.VariableNode))
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
