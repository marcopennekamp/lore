package lore.test.parser

import fastparse._
import lore.test.BaseSpec
import org.scalatest.Assertion
import ScalaWhitespace._
import lore.parser.Space

trait ParserSpecExtensions[Node <: lore.ast.Node] { base: BaseSpec =>
  def parser[_: P]: P[Node]

  implicit class CheckParseExtension(source: String) {
    private def parse[T](onSuccess: Node => T, onFailure: String => T): T = {
      def file[_: P] = P(Space.WL ~ parser ~ Space.WL ~ End)
      fastparse.parse(source, file(_)) match {
        case Parsed.Success(result, _) => onSuccess(result)
        case Parsed.Failure(_, _, extra) => onFailure(extra.trace().aggregateMsg)
      }
    }

    def -->(expected: Node): Assertion = {
      parse[Assertion](_ shouldEqual expected, message => fail(s"Couldn't parse $source. $message."))
    }

    def fails: Assertion = {
      parse[Assertion](result => fail(s"Parsing $source should have failed, but resulted in $result."), _ => succeed)
    }

    def parsed: Node = {
      parse[Node](identity, message => fail(s"Couldn't parse $source. $message."))
    }
  }
}
