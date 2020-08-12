package lore.compiler.phases.parsing

import fastparse.NoWhitespace._
import fastparse._

/**
  * Lexical objects are sensitive to whitespace, so we define them in this separate object.
  */
object LexicalParser {
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
    "abstract", "action", "class", "component", "entity", "extends", "function", "label", "mut", "owned by",
    "overrides", "type", "with",
  ))

  /**
    * A critical keyword is a keyword that, when used as an identifier in any place within the language, leads to
    * ambiguity or confusion. For example, some keywords may not be consistently able to stand as variable names,
    * such as return being possible in a declaration but not as a simple expression, as the parser will prefer to read
    * "return" as a return top-level expression, for example.
    *
    * Another example concerns the "construct" keyword: Declaring a function named "construct" is possible, while
    * calling said function is impossible since "construct" gets parsed as a construct call.
    */
  def criticalKeyword[_: P]: P[Unit] = P(StringIn(
    "const", "construct", "dynamic", "else", "false", "for", "if", "let", "return", "super", "this", "true",
    "while",
  ))

  def identifier[_: P]: P[String] = P(!criticalKeyword ~ (letter | "_") ~ (letter | digit | "_").rep).!
  def integer[_: P]: P[Int] = P(negatable[Int, Any](digitsNoTrailing.!.map(_.toInt)))
  def real[_: P]: P[Double] = P(negatable[Double, Any]((digitsNoTrailing ~ fraction).!.map(_.toDouble)))
}
