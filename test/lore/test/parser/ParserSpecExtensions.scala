package lore.test.parser

import fastparse._
import lore.test.BaseSpec
import org.scalatest.Assertion

trait ParserSpecExtensions[Node <: lore.ast.Node] { base: BaseSpec =>
  def parser: P[_] => P[Node]

  implicit class CheckParseExtension(source: String) {
    def -->(expected: Node): Assertion = {
      fastparse.parse(source, parser(_)) match {
        case Parsed.Success(result, _) => result shouldEqual expected
        case Parsed.Failure(_, _, extra) => fail(s"Couldn't parse the source $source. ${extra.trace().aggregateMsg}.")
      }
    }
  }
}
