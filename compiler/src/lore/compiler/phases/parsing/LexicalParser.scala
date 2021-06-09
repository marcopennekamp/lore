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

  def lowercase[_: P]: P[Unit]            = P(CharIn("a-z"))
  def uppercase[_: P]: P[Unit]            = P(CharIn("A-Z"))
  def letter[_: P]: P[Unit]               = P(lowercase | uppercase)
  def typeSpecialCharacter[_: P]: P[Unit] = P(CharIn("+"))
  def digit[_: P]: P[Unit]                = P(CharIn("0-9"))
  def hexDigit[_: P]: P[Unit]             = P(CharIn("0-9a-fA-F"))
  def nonZeroDigit[_: P]: P[Unit]         = P(CharIn("1-9"))
  def digitsNoTrailing[_: P]: P[Unit]     = P(nonZeroDigit ~ digit.rep | "0")
  def fraction[_: P]: P[Unit]             = P("." ~ digit.rep(1))

  /**
    * A keyword is a string that, when used as an identifier in any place within the language, leads to ambiguity or
    * confusion. For example, some keywords may not be consistently able to stand as variable names, such as `return`
    * being possible in a declaration but not as a simple expression, as the parser will prefer to read `return` as a
    * return top-level expression.
    *
    * Another example concerns the `dynamic` keyword: Declaring a function named "dynamic" would be possible, but
    * calling said function is impossible since `dynamic` gets parsed as a dynamic call.
    *
    * Some additional words may have special meaning to the parser, but aren't keywords, because the parser can resolve
    * the ambiguity in all instances. These words are: action, extends, function, implements, mut, struct, trait, and
    * type.
    */
  val keywords: Vector[String] = Vector(
    "dynamic", "else", "false", "for", "if", "let", "return", "super", "this", "true", "while",
  )

  def identifier[_: P]: P[String] = P((letter | "_") ~ (letter | digit | "_").rep).!.filter(!keywords.contains(_))
  def typeIdentifier[_: P]: P[String] = P((typeSpecialCharacter | letter | "_") ~ (typeSpecialCharacter | letter | digit | "_").rep).!.filter(!keywords.contains(_))
  def structIdentifier[_: P]: P[String] = P(identifier)
  def integer[_: P]: P[Long] = P(negatable[Long, Any](digitsNoTrailing.!.map(_.toLong)))
  def real[_: P]: P[Double] = P(negatable[Double, Any]((digitsNoTrailing ~ fraction).!.map(_.toDouble)))
}
