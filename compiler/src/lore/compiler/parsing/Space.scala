package lore.compiler.parsing

import fastparse.NoWhitespace._
import fastparse._

/**
  * Some of the whitespace handling code has been copied from Li Haoyi's scalaparse implementation.
  */
object Space {
  def whitespaces[_: P]: P[Unit] = P(NoTrace(CharsWhileIn("\u0020\u0009")))
  def newline[_: P]: P[Unit] = P(NoTrace(StringIn("\r\n", "\n")))

  /**
    * Scala-style comments. Note that these parser definitions need to line up with the implicit whitespace
    * parser, which is now ScalaWhitespace, but might change in the future when we introduce our own comment
    * syntax.
    */
  def commentChunk[_: P]: P[Unit] = P(CharsWhile(c => c != '/' && c != '*') | multilineComment | !"*/" ~ AnyChar)
  def multilineComment[_: P]: P[Unit] = P("/*" ~/ commentChunk.rep ~ "*/")
  def sameLineCharChunks[_: P]: P[Unit] = P(CharsWhile(c => c != '\n' && c != '\r') | !newline ~ AnyChar)
  def lineComment[_: P]: P[Unit] = P("//" ~ sameLineCharChunks.rep ~ &(newline | End))
  def comment[_: P]: P[Unit] = P(multilineComment | lineComment)

  /**
    * Parses whitespace except for newlines.
    */
  def WS[_: P]: P[Unit] = P(NoTrace((whitespaces | comment).rep))
  def WS1[_: P]: P[Unit] = P(NoTrace((whitespaces | comment).rep(1)))

  /**
    * Parses whitespace, including newlines. This is the default for most things.
    */
  def WL0[_: P]: P[Unit] = P(ScalaWhitespace.whitespace(P.current))
  def WL[_: P]: P[Unit] = P(NoCut(WL0))

  /**
    * Parses an expression terminator. For now, this can only be a newline.
    */
  def terminator[_: P]: P[Unit] = P(newline.rep(1))
  def terminators[_: P]: P[Unit] = P(NoTrace(NoCut(WS) ~ terminator.rep(1, NoCut(WS)) ~ NoCut(WS)) )
}
