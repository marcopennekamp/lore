package lore

import fastparse.ScalaWhitespace._
import fastparse._
import lore.compiler.phases.parsing.StatementParser

/**
  * This object can be run (for example from within the IDE) to parse a single statement (or other code if the parser
  * is changed) for debugging and logging purposes. Running tests with fastparse logging is messy as it provides way
  * too much useless data from tests that aren't relevant to the problem, but logged anyway. This object can be used,
  * in conjunction with P(...).log, to pinpoint issues with a parsing run.
  */
object SingleParse {
  private val source = "if ({ return 0 }) a else b"

  def main(args: Array[String]): Unit = {
    def file[_: P] = P(StatementParser.statement ~ End)
    fastparse.parse(source, file(_)) match {
      case Parsed.Success(result, _) => println(result)
      case Parsed.Failure(_, _, extra) => println(extra.trace().aggregateMsg)
    }
  }
}
