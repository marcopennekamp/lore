package lore.parser

import fastparse._
import NoWhitespace._

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
  def nonZeroDigit[_: P]: P[Unit]     = P(CharIn("1-9"))
  def digitsNoTrailing[_: P]: P[Unit] = P(nonZeroDigit ~ digit.rep | "0")
  def fraction[_: P]: P[Unit]         = P("." ~ digit.rep(1))

  def identifier[_: P]: P[String] = P((letter | "_") ~ (letter | digit | "_").rep).!
  def integer[_: P]: P[Int] = P(negatable[Int, Any](digitsNoTrailing.!.map(_.toInt)))
  def real[_: P]: P[Double] = P(negatable[Double, Any]((digitsNoTrailing ~ fraction).!.map(_.toDouble)))
}
