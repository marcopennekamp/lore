package lore.compiler.phases.parsing

import fastparse.NoWhitespace._
import fastparse._
import lore.ast._

/**
  * Lexical objects are sensitive to whitespace, so we define them in this separate object.
  */
object LexicalParser {
  import Node.withIndex

  // Taken from Li Haoyi's pythonparse. This is nifty. Thanks!
  def negatable[T, _: P](p: => P[T])(implicit ev: Numeric[T]): P[T] = (("+" | "-").?.! ~ p).map {
    case ("-", i) => ev.negate(i)
    case (_, i) => i
  }

  def lowercase[_: P]: P[Unit]        = P(CharIn("a-z"))
  def uppercase[_: P]: P[Unit]        = P(CharIn("A-Z"))
  def letter[_: P]: P[Unit]           = P(lowercase | uppercase)
  def digit[_: P]: P[Unit]            = P(CharIn("0-9"))
  def hexDigit[_: P]: P[Unit]         = P(CharIn("0-9a-fA-F"))
  def nonZeroDigit[_: P]: P[Unit]     = P(CharIn("1-9"))
  def digitsNoTrailing[_: P]: P[Unit] = P(nonZeroDigit ~ digit.rep | "0")
  def fraction[_: P]: P[Unit]         = P("." ~ digit.rep(1))

  /**
    * A keyword is a string that is used to signal some kind of specific language construct to the compiler. Not all
    * keywords are automatically disallowed as identifiers: only critical keywords have that restriction.
    */
  def keyword[_: P]: P[Unit] = P(criticalKeyword | StringIn(
    "abstract", "action", "class", "component", "entity", "extends", "function", "in", "label", "mut", "owned by",
    "overrides", "type", "with",
  ))

  /**
    * A critical keyword is a keyword that, when used as an identifier in any place within the language, leads to
    * ambiguity or confusion. For example, some keywords may not be consistently able to stand as variable names,
    * such as yield being possible in a declaration but not as a simple expression, as the parser will prefer to read
    * "yield" as a yield TLE, for example.
    *
    * Another example concerns the "construct" keyword: Declaring a function named "construct" is possible, while
    * calling said function is impossible since "construct" gets parsed as a construct call.
    */
  def criticalKeyword[_: P]: P[Unit] = P(StringIn(
    "const", "construct", "else", "false", "for", "if", "let", "repeat", "return", "super", "this", "true",
    "while", "yield",
  ))

  def identifier[_: P]: P[String] = P(!criticalKeyword ~ (letter | "_") ~ (letter | digit | "_").rep).!
  def integer[_: P]: P[Int] = P(negatable[Int, Any](digitsNoTrailing.!.map(_.toInt)))
  def real[_: P]: P[Double] = P(negatable[Double, Any]((digitsNoTrailing ~ fraction).!.map(_.toDouble)))

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Strings and interpolation! (Thanks again to Li Haoyi and his scalaparse example, which I used as a base for this.)
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def string[_: P]: P[ExprNode] = {
    // Helper to quickly gobble up large chunks of uninteresting characters. We break out conservatively, even if
    // we don't know it's a "real" escape sequence: worst come to worst it turns out to be a dud and we go back into
    // a CharsChunk next rep.
    def stringChars = P(CharsWhile(c => c != '\n' && c != '\'' && c != '\\' && c != '$').!)
    // The repetitions here attempt to shove as many characters into StringLiteralNode as possible.
    def notStringEnd = P((!CharIn("\n'") ~ AnyChar).!).map(ExprNode.StringLiteralNode)
    def content = P((stringChars | escape).rep(1)).map { strings =>
      ExprNode.StringLiteralNode(strings.foldLeft("")(_ + _))
    }
    // We have to check content, interpolation, notStringEnd exactly in this order, otherwise notStringEnd would
    // consume parts that are meant to be escapes or interpolations.
    P("'" ~ (content | interpolation | notStringEnd).rep ~ "'").map(_.toList).map {
      case List() => ExprNode.StringLiteralNode("")
      // This can either be a single string literal or any expression enclosed as such: '$expr'.
      case List(expression) => expression
      case strings => ExprNode.ConcatenationNode(strings)
    }
  }

  def interpolation[_: P]: P[ExprNode] = {
    def simple = P(Index ~ identifier).map(withIndex(ExprNode.VariableNode))
    def block = P("{" ~ NoCut(StatementParser.expression) ~ "}")
    P("$" ~ (block | simple))
  }

  def escape[_: P]: P[String] = {
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
