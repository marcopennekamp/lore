package lore.test.parser

import fastparse._
import lore.test.BaseSpec
import org.scalatest.Assertion

import ScalaWhitespace._

trait ParserSpecExtensions[Node <: lore.ast.Node] { base: BaseSpec =>
  def parser[_: P]: P[Node]

  implicit class CheckParseExtension(source: String) {
    private def parse(onSuccess: Node => Assertion, onFailure: String => Assertion): Assertion = {
      def file[_: P] = P(parser ~ End)
      fastparse.parse(source, file(_)) match {
        case Parsed.Success(result, _) => onSuccess(result)
        case Parsed.Failure(_, _, extra) => onFailure(extra.trace().aggregateMsg)
      }
    }

    def -->(expected: Node): Assertion = {
      parse(_ shouldEqual expected, message => fail(s"Couldn't parse $source. ${message}."))
    }

    def fails: Assertion = {
      parse(result => fail(s"Parsing $source should have failed, but resulted in $result."), message => succeed)
    }
  }
}
