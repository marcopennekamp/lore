package lore.parser

import fastparse._

trait IgnoreWhitespace {
  implicit val whitespace = { implicit ctx: ParsingRun[_] =>
    NoTrace(CharsWhileIn(" \t", 0))
  }
}
